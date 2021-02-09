{-# LANGUAGE OverloadedStrings #-}

module Main where

import GitHub (github)
import qualified GitHub

main :: IO ()
main = do
    let aut = ()    -- () means no authentifikation
        --token = GitHub.OAuth $ error "some bytestring"    -- probably best to read from file
    issues <- github aut $ GitHub.issuesForRepoR "spamviech" "Zugkontrolle" mempty GitHub.FetchAll
    -- createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request RW Issue
    -- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
    print issues
-- curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/spamviech/Zugkontrolle/issues
-- https://docs.github.com/en/rest/reference/issues#list-repository-issues
-- https://github.com/phadej/github/tree/master/samples/Issues
