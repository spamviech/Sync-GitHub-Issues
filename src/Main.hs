{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as Vector
--import qualified Data.ByteString as ByteString
import GitHub (github, issuesForRepoR, Issue(..), createIssueR, editIssueR, NewIssue(..))
import qualified GitHub
import Token (token)

main :: IO ()
main = do
    let aut = GitHub.OAuth token
    -- issues <- github aut
    --     $ GitHub.issuesForRepoR "spamviech" "Sync-Github-Issues" mempty GitHub.FetchAll
    -- print issues
    newIssue <- github aut
        $ createIssueR "spamviech" "Sync-Github-Issues"
        $ NewIssue
        { newIssueTitle = "Test"
        , newIssueBody = Nothing
        , newIssueAssignees = Vector.empty
        , newIssueMilestone = Nothing
        , newIssueLabels = Nothing
        }
    print newIssue
-- createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request RW Issue
-- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
-- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
-- https://docs.github.com/en/rest/reference/issues#list-repository-issues
-- https://github.com/phadej/github/tree/master/samples/Issues
