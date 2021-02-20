{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Control.Monad (forM, foldM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT, throwE)
import Data.Bifunctor (Bifunctor(bimap, first, second))
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.List (foldl', maximumBy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import ExitCodes (ExitCode(..), exitWith)
-- import qualified Data.ByteString as ByteString
import GitHub (github, Issue(..), IssueComment(..), NewIssue(..))
import qualified GitHub
import Lens.Micro (_5)
import Lens.Micro.Extras (view)
import LocalCopy
       (LocalIssue(..), LocalComment(..), readIssues, writeIssues, handleExceptT, showText)
import Repository (Repository(..), parseRepositoryInformation)
import System.IO (stderr)

exitExceptT :: ExceptT (ExitCode, Text) IO () -> IO ()
exitExceptT action = runExceptT action >>= \case
    (Right ()) -> exitWith Success
    (Left (exitCode, errorMessage)) -> do
        Text.hPutStrLn stderr errorMessage
        exitWith exitCode

main :: IO ()
main = exitExceptT $ do
    let tokenPath = ".githubtoken"
        tokenMissing = "No Token found in \"" <> Text.pack tokenPath <> "\"."
    aut <- handleExceptT (\_e -> throwE (NoTokenError, tokenMissing))
        $ lift
        $ GitHub.OAuth <$> ByteString.readFile tokenPath
    Repository {owner, repository, filePath} <- lift parseRepositoryInformation
    lift $ putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    localIssues <- withExceptT ((ParseFileError, ) . Text.pack) $ readIssues filePath
    remoteIssues <- if queryGitHub
        then withExceptT ((ConnectionError, ) . showText)
            $ queryIssues aut owner repository
            $ HashMap.keys (fst $ fst localIssues) ++ HashMap.keys (fst $ snd localIssues)
        else pure (HashMap.empty, HashMap.empty)
    syncedIssues <- if updateGitHub
        then withExceptT ((ConnectionError, ) . showText)
            $ applyRemoteChanges aut
            $ calculateChanges remoteIssues localIssues
        else pure $ view _5 $ calculateChanges remoteIssues localIssues
    if writeToFile
        then withExceptT (ParseFileError, ) $ writeIssues filePath syncedIssues
        else lift $ print syncedIssues
    where
        -- TODO Dev-Flags so file-overwrites/api-access can be restricted
        queryGitHub = False

        updateGitHub = False

        writeToFile = False

-- | Ask /github.com/ for all currently open issues.
queryIssues :: (GitHub.AuthMethod auth)
            => auth
            -> GitHub.Name GitHub.Owner
            -> GitHub.Name GitHub.Repo
            -> [GitHub.IssueNumber]
            -> ExceptT
                GitHub.Error
                IO
                (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository knownIssueNumbers = do
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- ExceptT $ github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    openIssues <- HashMap.fromList <$> forM (Vector.toList issues) localIssue
    foldM lookupIssue (openIssues, HashMap.empty) knownIssueNumbers
    where
        lfNewlines :: Text -> Text
        lfNewlines = Text.replace "\r\n" "\n"

        localIssue :: GitHub.Issue -> ExceptT GitHub.Error IO (GitHub.IssueNumber, LocalIssue)
        localIssue Issue {issueNumber, issueTitle, issueBody, issueUpdatedAt} = do
            -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
            let idBodyTuple IssueComment {issueCommentId, issueCommentBody, issueCommentUpdatedAt} =
                    ( issueCommentId
                    , LocalComment
                      { comment = lfNewlines issueCommentBody
                      , modificationTime = issueCommentUpdatedAt
                      }
                    )
            comments <- fmap (HashMap.fromList . map idBodyTuple . Vector.toList)
                $ ExceptT
                $ github aut (GitHub.commentsR owner repository issueNumber GitHub.FetchAll)
            pure
                ( issueNumber
                , LocalIssue
                  { title = lfNewlines issueTitle
                  , modificationTime = issueUpdatedAt
                  , body = lfNewlines <$> issueBody
                  , comments
                  }
                )

        lookupIssue
            :: (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
            -> GitHub.IssueNumber
            -> ExceptT
                GitHub.Error
                IO
                (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
        lookupIssue acc@(openIssues, closedIssues) issueNumber
            | HashMap.member issueNumber openIssues = pure acc
            | otherwise = do
                -- issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
                issue <- ExceptT (github aut $ GitHub.issueR owner repository issueNumber)
                (openIssues, ) . flip (uncurry HashMap.insert) closedIssues <$> localIssue issue

type ChangeInformation =
    ( [GitHub.EditIssue]                             -- editIssueR
    , [(GitHub.NewIssue, [Text], GitHub.IssueState)] -- createIssueR, createCommentR, editIssueR
    , [(GitHub.Id GitHub.Comment, Text)]             -- editCommentR
    , [(GitHub.IssueNumber, Text)]                   -- createCommentR
    , (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue) -- localUnchangedIssues
    )

data LocalIssueState = LocalIssueState { state :: GitHub.IssueState, localIssue :: LocalIssue }
    deriving (Show, Eq)

instance Semigroup LocalIssueState where
    (<>) :: LocalIssueState -> LocalIssueState -> LocalIssueState
    (<>)
        LocalIssueState
        {localIssue = i0@LocalIssue {modificationTime = t0, comments = c0}, state = s0}
        LocalIssueState
        {localIssue = i1@LocalIssue {modificationTime = t1, comments = c1}, state = s1}
        | t0 < t1 = LocalIssueState { localIssue = i1 { comments }, state = s1 }
        | otherwise = LocalIssueState { localIssue = i0 { comments }, state = s0 }
        where
            comments :: HashMap Int LocalComment
            comments = HashMap.unionWith (curry $ maximumBy compareByTime) c0 c1

            compareByTime :: LocalComment -> LocalComment -> Ordering
            compareByTime
                LocalComment {modificationTime = tc0}
                LocalComment {modificationTime = tc1} = compare tc0 tc1

type RemoteLocalIssueState = (Maybe LocalIssueState, Maybe LocalIssueState)

-- | Calculate required changes to achieve a synced state.
calculateChanges
    :: (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
    -> ( (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
       , (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
       )
    -> ChangeInformation
calculateChanges
    (remoteOpenIssues, remoteClosedIssues)
    ((knownLocalOpenIssues, newLocalOpenIssues), (knownLocalClosedIssues, newLocalClosedIssues)) =
    (editIssues, newIssues, editComments, newComments, unchangedIssues) --TODO
    where
        newIssues :: [(GitHub.NewIssue, [Text], GitHub.IssueState)]
        newIssues =
            newIssuesWithState GitHub.StateOpen newLocalOpenIssues
            ++ newIssuesWithState GitHub.StateClosed newLocalClosedIssues

        -- new Issue can't have known comments
        -- thus, they will be created as well, probably assigning a new ID in the process
        newIssuesWithState :: GitHub.IssueState
                           -> [(LocalIssue, [LocalComment])]
                           -> [(GitHub.NewIssue, [Text], GitHub.IssueState)]
        newIssuesWithState state newLocalIssues =
            [( NewIssue
               { newIssueTitle = title
               , newIssueBody = body
               , newIssueAssignees = Vector.empty
               , newIssueMilestone = Nothing
               , newIssueLabels = Nothing
               }
             , map comment $ HashMap.elems comments ++ newIssueComments
             , state
             ) | (LocalIssue {title, body, comments}, newIssueComments) <- newLocalIssues]

        newComments :: [(GitHub.IssueNumber, Text)]
        newComments = go knownLocalOpenIssues ++ go knownLocalClosedIssues
            where
                go :: HashMap GitHub.IssueNumber (LocalIssue, [LocalComment])
                   -> [(GitHub.IssueNumber, Text)]
                go knownLocalIssues =
                    concat
                    $ HashMap.elems
                    $ HashMap.mapWithKey newCommentsWithIssueNumber knownLocalIssues

                newCommentsWithIssueNumber
                    :: GitHub.IssueNumber -> (a, [LocalComment]) -> [(GitHub.IssueNumber, Text)]
                newCommentsWithIssueNumber issueNumber = map ((issueNumber, ) . comment) . snd

        editIssues :: [GitHub.EditIssue]
        editComments :: [(GitHub.Id GitHub.Comment, Text)]
        unchangedIssues
            :: (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
        (editIssues, editComments, bimap HashMap.fromList HashMap.fromList -> unchangedIssues) =
            -- remoteOpenIssues, remoteClosedIssues
            -- knownLocalOpenIssues, knownLocalClosedIssues
            foldl' syncIssue ([], [], ([], []))
            $ HashMap.toList
            $ unionsWith
                (<>)
                [ HashMap.map (mapRemote GitHub.StateOpen) remoteOpenIssues
                , HashMap.map (mapRemote GitHub.StateClosed) remoteClosedIssues
                , HashMap.map (mapLocal GitHub.StateOpen . fst) knownLocalOpenIssues
                , HashMap.map (mapLocal GitHub.StateClosed . fst) knownLocalClosedIssues]

        mapRemote :: GitHub.IssueState -> LocalIssue -> RemoteLocalIssueState
        mapRemote state = (, Nothing) . Just . LocalIssueState state

        mapLocal :: GitHub.IssueState -> LocalIssue -> RemoteLocalIssueState
        mapLocal state = (Nothing, ) . Just . LocalIssueState state

        unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [HashMap k v] -> HashMap k v
        unionsWith f = foldl' (HashMap.unionWith f) HashMap.empty

        syncIssue :: ( [GitHub.EditIssue]
                     , [(GitHub.Id GitHub.Comment, Text)]
                     , ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
                     )
                  -> (GitHub.IssueNumber, RemoteLocalIssueState)
                  -> ( [GitHub.EditIssue]
                     , [(GitHub.Id GitHub.Comment, Text)]
                     , ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
                     )
        syncIssue
            (editIssues', editComments', (unchangedOpenIssues, unchangedClosedIssues))
            (issueNumber, (Just remoteIssue, Just localIssue)) = undefined -- TODO
        syncIssue
            (editIssues', editComments', unchangedIssues')
            (issueNumber, (Just LocalIssueState {state, localIssue}, Nothing)) =
            (editIssues', editComments', addNewRemoteIssue unchangedIssues')
            where
                addNewRemoteIssue
                    :: ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
                    -> ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
                addNewRemoteIssue = bimapPart ((issueNumber, localIssue) :)

                bimapPart :: (a -> a) -> (a, a) -> (a, a)
                bimapPart
                    | GitHub.StateOpen <- state = first
                    | GitHub.StateClosed <- state = second
        syncIssue acc (_issueNumber, (Nothing, _localIssue)) = acc  -- deleted issue?

-- | Apply changes from 'calculateChanges' to /github.com/ and return the updated 'LocalIssues's.
applyRemoteChanges
    :: (GitHub.AuthMethod auth)
    => auth
    -> ChangeInformation
    -> ExceptT
        GitHub.Error
        IO
        (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
applyRemoteChanges aut changes = do
    -- TODO create new issues
    -- createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request RW Issue
    -- newIssue <- github aut
    --     $ GitHub.createIssueR owner repository
    --     $ NewIssue
    --     { newIssueTitle = "Test"
    --     , newIssueBody = Nothing
    --     , newIssueAssignees = Vector.empty
    --     , newIssueMilestone = Nothing
    --     , newIssueLabels = Nothing
    --     }
    -- print newIssue
    -- TODO edit changed issues
    -- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
    -- editOfIssue :: EditIssue
    -- createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request RW Comment
    -- editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request RW Comment
    -- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
    -- https://docs.github.com/en/rest/reference/issues#list-repository-issues
    -- https://github.com/phadej/github/tree/master/samples/Issues
    undefined --TODO
