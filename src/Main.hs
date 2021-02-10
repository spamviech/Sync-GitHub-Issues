{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Either (fromRight)
import qualified Data.Vector as Vector
-- import qualified Data.ByteString as ByteString
import GitHub (github  {-Issue(..), NewIssue(..),, IssueNumber(..)-})
import qualified GitHub
import Repository (Repository(..), parseRepositoryInformation)
import System.Directory
import Token (token)

main :: IO ()
main = do
    Repository {owner, repository, directory} <- parseRepositoryInformation
    let aut = GitHub.OAuth token
    putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    createDirectoryIfMissing False directory
    files <- listDirectory directory
    putStrLn $ "---------------\nfiles: " ++ show files
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    putStrLn "---------------\nIssues:"
    mapM_ print $ fromRight Vector.empty issues
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
-- issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
-- issue <- github aut $ GitHub.issueR owner repository $ IssueNumber 3
-- print issue
-- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
-- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
-- https://docs.github.com/en/rest/reference/issues#list-repository-issues
-- https://github.com/phadej/github/tree/master/samples/Issues
