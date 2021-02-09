{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (forM_)
-- import qualified Data.Vector as Vector
-- import qualified Data.ByteString as ByteString
import GitHub (github  {-Issue(..), NewIssue(..),, IssueNumber(..)-})
import qualified GitHub
import Options.Applicative (help, info, long, metavar, progDesc, short, showDefault, strOption
                          , value, execParser, helper, Parser)
import System.Directory
import Token (token)

data Repository =
    Repository
    { owner :: GitHub.Name GitHub.Owner
    , repository :: GitHub.Name GitHub.Repo
    , directory :: FilePath
    }
    deriving (Show, Eq)

repositoryOption :: Parser Repository
repositoryOption = Repository <$> ownerOption <*> repoOption <*> directoryOption

ownerOption :: Parser (GitHub.Name GitHub.Owner)
ownerOption =
    strOption $ long "owner" <> short 'o' <> metavar "OWNER" <> help "owner of the repository"

repoOption :: Parser (GitHub.Name GitHub.Repo)
repoOption =
    strOption
    $ long "repository" <> short 'r' <> metavar "REPOSITORY" <> help "name of the repository"

directoryOption :: Parser FilePath
directoryOption =
    strOption
    $ long "directory"
    <> short 'd'
    <> metavar "DIRECTORY"
    <> help "directory where to store the local copy"
    <> value "Issues"
    <> showDefault

main :: IO ()
main = do
    Repository {owner, repository, directory} <- execParser
        $ info (helper <*> repositoryOption)
        $ progDesc "Sync issues of the specified github.com repository with a local copy."
    let aut = GitHub.OAuth token
    createDirectoryIfMissing False directory
    files <- listDirectory directory
    print files
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    forM_ issues print
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
