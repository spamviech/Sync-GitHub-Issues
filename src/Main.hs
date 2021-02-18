{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Control.Exception as Exception
import Control.Monad (forM, when)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import ExitCodes (ExitCode(..), exitWith)
-- import qualified Data.ByteString as ByteString
import GitHub (github, Issue(..), IssueComment(..)  {-, NewIssue(..), IssueNumber(..)-})
import qualified GitHub
import LocalCopy (LocalIssue(..), LocalComment(..), readIssues, writeIssues)
import Repository (Repository(..), parseRepositoryInformation)
import System.IO (stderr, hPutStrLn, hPrint)

main :: IO ()
main = do
    let tokenPath = ".githubtoken"
        tokenMissing = "No Token found in \"" ++ tokenPath ++ "\"."
    aut <- Exception.handle
        (\(_e :: Exception.IOException) -> hPutStrLn stderr tokenMissing >> exitWith NoTokenError)
        $ GitHub.OAuth <$> ByteString.readFile tokenPath
    Repository {owner, repository, filePath} <- parseRepositoryInformation
    putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    remoteIssues <- if queryGitHub
        then runExceptT (queryIssues aut owner repository) >>= \case
            Left err -> do
                hPrint stderr err
                exitWith ConnectionError
            Right issues -> pure issues
        else pure HashMap.empty
    (localOpenIssues, localClosedIssues) <- runExceptT (readIssues filePath) >>= \case
        Left err -> do
            hPutStrLn stderr $ "Parse Error: " ++ err
            exitWith ParseFileError
        Right issues -> pure issues
    print localOpenIssues
    print localClosedIssues
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
    -- TODO write file Issues.txt
    -- let syncedIssues = (remoteIssues, HashMap.empty)
    let syncedIssues =
            (HashMap.map fst $ fst localOpenIssues, HashMap.map fst $ fst localClosedIssues)
    -- let writeFailure = "Failed to write to \"" ++ filePath ++ "\""
    when writeToFile $ writeIssues filePath syncedIssues
    exitWith Success
    where
        -- TODO Dev-Flags so file-overwrites/api-access can be restricted
        queryGitHub = False

        writeToFile = False

queryIssues :: (GitHub.AuthMethod auth)
            => auth
            -> GitHub.Name GitHub.Owner
            -> GitHub.Name GitHub.Repo
            -> ExceptT GitHub.Error IO (HashMap GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository = do
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- ExceptT $ github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    fmap HashMap.fromList
        $ forM (Vector.toList issues)
        $ \Issue {issueNumber, issueTitle, issueBody} -> do
            -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
            let idBodyTuple c =
                    (issueCommentId c, LocalComment { comment = lfNewlines $ issueCommentBody c })
            comments <- fmap (HashMap.fromList . map idBodyTuple . Vector.toList)
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

calculateChanges :: ( [GitHub.EditIssue]    -- editIssueR
                    , [GitHub.NewIssue]     -- createIssueR
                    , [(GitHub.Id GitHub.Comment, Text)]    -- editCommentR
                    , [(GitHub.IssueNumber, Text)]          -- createCommentR
                    , ( HashMap GitHub.IssueNumber LocalIssue
                      , HashMap GitHub.IssueNumber LocalIssue
                      ) -- syncedIssues
                    )
calculateChanges = undefined --TODO
