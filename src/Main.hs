{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (forM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT, throwE)
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime())
import qualified Data.Vector as Vector
import ExitCodes (ExitCode(..), exitWith)
-- import qualified Data.ByteString as ByteString
import GitHub (github, Issue(..), IssueComment(..), NewIssue(..))
import qualified GitHub
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
    remoteIssues <- if queryGitHub
        then queryIssues aut owner repository
        else pure HashMap.empty
    localIssues <- readIssues filePath
    syncedIssues <- if updateGitHub
        then applyRemoteChanges aut $ calculateChanges remoteIssues localIssues
        else pure $ (\(_a, _b, _c_, _d, e) -> e) $ calculateChanges remoteIssues localIssues
    if writeToFile
        then writeIssues filePath syncedIssues
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
            -> ExceptT (ExitCode, Text) IO (HashMap GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository = do
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- withExceptT ((ConnectionError, ) . showText)
        $ ExceptT
        $ github aut
        $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    fmap HashMap.fromList
        $ forM (Vector.toList issues)
        $ \Issue {issueNumber, issueTitle, issueBody} -> do
            -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
            let idBodyTuple c =
                    (issueCommentId c, LocalComment { comment = lfNewlines $ issueCommentBody c })
            comments <- fmap (HashMap.fromList . map idBodyTuple . Vector.toList)
                $ withExceptT ((ConnectionError, ) . showText)
                $ ExceptT
                $ github aut (GitHub.commentsR owner repository issueNumber GitHub.FetchAll)
            pure
                ( issueNumber
                , LocalIssue
                  { title = lfNewlines issueTitle, body = lfNewlines <$> issueBody, comments }
                )
    where
        lfNewlines :: Text -> Text
        lfNewlines = Text.replace "\r\n" "\n"

-- | Calculate required changes to achieve a synced state.
calculateChanges
    :: HashMap GitHub.IssueNumber LocalIssue
    -> ( UTCTime
       , (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
       , (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
       )
    -> ( [GitHub.EditIssue]                             -- editIssueR
       , [(GitHub.NewIssue, [Text], GitHub.IssueState)] -- createIssueR, createCommentR
       , [(GitHub.Id GitHub.Comment, Text)]             -- editCommentR
       , [(GitHub.IssueNumber, Text)]                   -- createCommentR
       , (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue) -- localUnchangedIssues
       )
calculateChanges
    remoteOpenIssues
    ( modificationTime
    , (knownLocalOpenIssues, newLocalOpenIssues)
    , (knownLocalClosedIssues, newLocalClosedIssues)
    ) = (editIssues, newIssues, editComments, newComments, localUnchangedIssues) --TODO
    where
        editIssues :: [GitHub.EditIssue]
        editIssues = undefined

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

        editComments :: [(GitHub.Id GitHub.Comment, Text)]
        editComments = undefined

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

        localUnchangedIssues
            :: (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
        localUnchangedIssues = undefined

-- | Apply changes from 'calculateChanges' to /github.com/ and return the updated 'LocalIssues's.
applyRemoteChanges
    :: (GitHub.AuthMethod auth)
    => auth
    -> ( [GitHub.EditIssue]                             -- editIssueR
       , [(GitHub.NewIssue, [Text], GitHub.IssueState)] -- createIssueR, createCommentR
       , [(GitHub.Id GitHub.Comment, Text)]             -- editCommentR
       , [(GitHub.IssueNumber, Text)]                   -- createCommentR
       , (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue) -- localUnchangedIssues
       )
    -> ExceptT
        (ExitCode, Text)
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
    -- TODO edit changed issues
    -- print newIssue
    -- issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
    -- issue <- github aut $ GitHub.issueR owner repository $ IssueNumber 3
    -- print issue
    -- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
    -- editOfIssue :: EditIssue
    -- createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request RW Comment
    -- editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request RW Comment
    -- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
    -- https://docs.github.com/en/rest/reference/issues#list-repository-issues
    -- https://github.com/phadej/github/tree/master/samples/Issues
    undefined --TODO
