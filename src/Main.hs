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
import Data.List (foldl', maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
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
            $ Map.keys (fst $ fst localIssues) ++ Map.keys (fst $ snd localIssues)
        else pure (Map.empty, Map.empty)
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
                (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository knownIssueNumbers = do
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- ExceptT $ github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    openIssues <- Map.fromList <$> forM (Vector.toList issues) localIssue
    foldM lookupIssue (openIssues, Map.empty) knownIssueNumbers
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
            comments <- fmap (Map.fromList . map idBodyTuple . Vector.toList)
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

        lookupIssue :: (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
                    -> GitHub.IssueNumber
                    -> ExceptT
                        GitHub.Error
                        IO
                        (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
        lookupIssue acc@(openIssues, closedIssues) issueNumber
            | Map.member issueNumber openIssues = pure acc
            | otherwise = do
                -- issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
                issue <- ExceptT (github aut $ GitHub.issueR owner repository issueNumber)
                (openIssues, ) . flip (uncurry Map.insert) closedIssues <$> localIssue issue

type ChangeInformation =
    ( [GitHub.EditIssue]                             -- editIssueR
    , [(GitHub.NewIssue, [Text], GitHub.IssueState)] -- createIssueR, createCommentR, editIssueR
    , [(GitHub.Id GitHub.Comment, Text)]             -- editCommentR
    , [(GitHub.IssueNumber, Text)]                   -- createCommentR
    , (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue) -- localUnchangedIssues
    )

data IssueState = IssueState { state :: GitHub.IssueState, issue :: LocalIssue }
    deriving (Show, Eq)

instance Semigroup IssueState where
    (<>) :: IssueState -> IssueState -> IssueState
    (<>)
        IssueState {issue = i0@LocalIssue {modificationTime = t0, comments = c0}, state = s0}
        IssueState {issue = i1@LocalIssue {modificationTime = t1, comments = c1}, state = s1}
        | t0 < t1 = IssueState { issue = i1 { comments }, state = s1 }
        | otherwise = IssueState { issue = i0 { comments }, state = s0 }
        where
            comments :: Map Int LocalComment
            comments = Map.unionWith (curry $ maximumBy compareByTime) c0 c1

            compareByTime :: LocalComment -> LocalComment -> Ordering
            compareByTime
                LocalComment {modificationTime = tc0}
                LocalComment {modificationTime = tc1} = compare tc0 tc1

type RemoteLocalIssueState = (Maybe IssueState, Maybe IssueState)

-- | Calculate required changes to achieve a synced state.
calculateChanges
    :: (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
    -> ( (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
       , (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
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
             , map comment $ Map.elems comments ++ newIssueComments
             , state
             ) | (LocalIssue {title, body, comments}, newIssueComments) <- newLocalIssues]

        newComments :: [(GitHub.IssueNumber, Text)]
        newComments = go knownLocalOpenIssues ++ go knownLocalClosedIssues
            where
                go :: Map GitHub.IssueNumber (LocalIssue, [LocalComment])
                   -> [(GitHub.IssueNumber, Text)]
                go knownLocalIssues =
                    concat $ Map.elems $ Map.mapWithKey newCommentsWithIssueNumber knownLocalIssues

                newCommentsWithIssueNumber
                    :: GitHub.IssueNumber -> (a, [LocalComment]) -> [(GitHub.IssueNumber, Text)]
                newCommentsWithIssueNumber issueNumber = map ((issueNumber, ) . comment) . snd

        editIssues :: [GitHub.EditIssue]
        editComments :: [(GitHub.Id GitHub.Comment, Text)]
        unchangedIssues :: (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
        (editIssues, editComments, bimap Map.fromList Map.fromList -> unchangedIssues) =
            -- remoteOpenIssues, remoteClosedIssues
            -- knownLocalOpenIssues, knownLocalClosedIssues
            foldl' syncIssue ([], [], ([], []))
            $ Map.toList
            $ unionsWith
                (<>)
                [ Map.map (mapRemote GitHub.StateOpen) remoteOpenIssues
                , Map.map (mapRemote GitHub.StateClosed) remoteClosedIssues
                , Map.map (mapLocal GitHub.StateOpen . fst) knownLocalOpenIssues
                , Map.map (mapLocal GitHub.StateClosed . fst) knownLocalClosedIssues]

        mapRemote :: GitHub.IssueState -> LocalIssue -> RemoteLocalIssueState
        mapRemote state = (, Nothing) . Just . IssueState state

        mapLocal :: GitHub.IssueState -> LocalIssue -> RemoteLocalIssueState
        mapLocal state = (Nothing, ) . Just . IssueState state

        unionsWith :: (Eq k, Ord k) => (v -> v -> v) -> [Map k v] -> Map k v
        unionsWith f = foldl' (Map.unionWith f) Map.empty

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
            (editIssues', editComments', unchangedIssues')
            ( issueNumber
            , ( Just
                    IssueState { state = remoteState
                               , issue = remoteIssue@LocalIssue { title = remoteTitle
                                                                , modificationTime = remoteTime
                                                                , body = remoteBody
                                                                , comments = remoteComments}}
              , Just
                    IssueState { state = localState
                               , issue = LocalIssue { title = localTitle
                                                    , modificationTime = localTime
                                                    , body = localBody
                                                    , comments = localComments}}
              )
            )
            | localTime < remoteTime
                || (remoteState == localState
                    && remoteTitle == localTitle
                    && remoteBody == localBody) =
                ( editIssues'
                , editComments'' ++ editComments'
                , addUnchangedIssue
                      issueNumber
                      remoteState
                      remoteIssue { comments }
                      unchangedIssues'
                )
            | otherwise =
                (editIssue : editIssues', editComments'' ++ editComments', unchangedIssues')
            where
                editComments'' :: [(GitHub.Id GitHub.Comment, Text)]
                comments :: Map Int LocalComment
                (editComments'', Map.fromList -> comments) =
                    foldl' syncComment ([], [])
                    $ Map.toList
                    $ Map.unionWith
                        (<>)
                        (Map.map ((, Nothing) . Just) remoteComments)
                        (Map.map ((Nothing, ) . Just) localComments)

                syncComment :: ([(GitHub.Id GitHub.Comment, Text)], [(Int, LocalComment)])
                            -> (Int, (Maybe LocalComment, Maybe LocalComment))
                            -> ([(GitHub.Id GitHub.Comment, Text)], [(Int, LocalComment)])
                syncComment
                    acc
                    ( commentId
                    , ( Just
                            remoteComment@LocalComment
                            {comment = remoteText, modificationTime = remoteCommentTime}
                      , Just
                            LocalComment {comment = localText, modificationTime = localCommentTime}
                      )
                    )
                    | localCommentTime < remoteCommentTime || remoteText == localText =
                        second ((commentId, remoteComment) :) acc
                    | otherwise = first ((GitHub.mkId Proxy commentId, localText) :) acc
                syncComment acc (commentId, (Just remoteComment, Nothing)) =
                    second ((commentId, remoteComment) :) acc
                syncComment acc (_commentId, (Nothing, _localComment)) = acc  -- deleted comment?

                editIssue :: GitHub.EditIssue
                editIssue =
                    GitHub.EditIssue
                    { editIssueTitle = Just localTitle
                    , editIssueBody = Just $ fromMaybe "" localBody
                    , editIssueAssignees = Nothing
                    , editIssueState = Just localState
                    , editIssueMilestone = Nothing
                    , editIssueLabels = Nothing
                    }
        syncIssue
            (editIssues', editComments', unchangedIssues')
            (issueNumber, (Just IssueState {state, issue}, Nothing)) =
            ( editIssues'
            , editComments'
            , addUnchangedIssue issueNumber state issue unchangedIssues'
            )
        syncIssue acc (_issueNumber, (Nothing, _localIssue)) = acc  -- deleted issue?

        addUnchangedIssue
            :: GitHub.IssueNumber
            -> GitHub.IssueState
            -> LocalIssue
            -> ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
            -> ([(GitHub.IssueNumber, LocalIssue)], [(GitHub.IssueNumber, LocalIssue)])
        addUnchangedIssue issueNumber state issue = bimapPart state ((issueNumber, issue) :)

        bimapPart :: GitHub.IssueState -> (a -> a) -> (a, a) -> (a, a)
        bimapPart GitHub.StateOpen = first
        bimapPart GitHub.StateClosed = second

-- | Apply changes from 'calculateChanges' to /github.com/ and return the updated 'LocalIssues's.
applyRemoteChanges
    :: (GitHub.AuthMethod auth)
    => auth
    -> ChangeInformation
    -> ExceptT
        GitHub.Error
        IO
        (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
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
