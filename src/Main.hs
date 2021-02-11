{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Control.Exception as Exception
import qualified Control.Monad (foldM)
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Either (fromRight)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import ExitCodes (connectionError)
-- import qualified Data.ByteString as ByteString
import GitHub (github  {-Issue(..), NewIssue(..),, IssueNumber(..)-})
import qualified GitHub
import Repository (Repository(..), parseRepositoryInformation)
import System.Directory
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)
import System.IO
import Token (token)

{-
Format:
- Eine Datei "Issues.txt" (macht nur Sinn bei überschaubarer Issue-Anzahl)
- Erst offene Issues bis Trennzeile (_______________________________), danach geschlossene Issues
- Issue-Format:
    #Issue: <Issue-Number>
    <Titel>
    <Leerzeile>
    <Body>
    ~~~~~~~~~~~
    #Comment: <Comment-Number>
    <Comment>
    -------------------------
    #Issue: <Issue-Number>
-}
main :: IO ()
main = do
    Repository {owner, repository, filePath} <- parseRepositoryInformation
    let aut = GitHub.OAuth token
    putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    issueFileContents <- Exception.handle (\(_e :: Exception.IOException) -> pure Text.empty)
        $ withFile filePath ReadMode
        $ \handle -> do
            hSetEncoding handle utf8
            hSetNewlineMode handle noNewlineTranslation
            Text.hGetContents handle
    print issueFileContents
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    github aut (GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll) >>= \case
        Left err -> do
            hPrint stderr err
            exitWith $ ExitFailure connectionError
        Right issues -> do
            -- TODO
            -- clean up directory
            -- include files in directoryNew, sort by new/changed
            -- create new files for current issues
            mapM_ print issues
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
    -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment) 
    -- createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request RW Comment
    -- editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request RW Comment
    -- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
    -- https://docs.github.com/en/rest/reference/issues#list-repository-issues
    -- https://github.com/phadej/github/tree/master/samples/Issues
    -- TODO clean up directoryNew
    exitSuccess
