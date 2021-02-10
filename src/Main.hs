{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as Text
-- import qualified Data.Vector as Vector
-- import qualified Data.ByteString as ByteString
import GitHub (github  {-Issue(..), NewIssue(..),, IssueNumber(..)-})
import qualified GitHub
import Options.Applicative (help, info, long, metavar, progDesc, short, showDefault, strOption
                          , value, execParser, helper, Parser())
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import Text.GitConfig.Parser (parseConfig, Section(..), GitConfig)
import Text.Megaparsec ()
import Token (token)

data Repository =
    Repository
    { owner :: GitHub.Name GitHub.Owner
    , repository :: GitHub.Name GitHub.Repo
    , directory :: FilePath
    }
    deriving (Show, Eq)

repositoryOption
    :: Maybe (GitHub.Name GitHub.Owner) -> Maybe (GitHub.Name GitHub.Repo) -> Parser Repository
repositoryOption localOwner localName =
    Repository <$> ownerOption localOwner <*> nameOption localName <*> directoryOption

ownerOption :: Maybe (GitHub.Name GitHub.Owner) -> Parser (GitHub.Name GitHub.Owner)
ownerOption remoteOwner =
    strOption
    $ long "owner"
    <> short 'o'
    <> metavar "OWNER"
    <> help "owner of the repository"
    <> maybe mempty value remoteOwner

nameOption :: Maybe (GitHub.Name GitHub.Repo) -> Parser (GitHub.Name GitHub.Repo)
nameOption remoteRepository =
    strOption
    $ long "repository"
    <> short 'r'
    <> metavar "REPOSITORY"
    <> help "name of the repository"
    <> maybe mempty value remoteRepository

directoryOption :: Parser FilePath
directoryOption =
    strOption
    $ long "directory"
    <> short 'd'
    <> metavar "DIRECTORY"
    <> help "directory where to store the local copy"
    <> value "Issues"
    <> showDefault

extractFromConfig
    :: Either e GitConfig -> (Maybe (GitHub.Name GitHub.Owner), Maybe (GitHub.Name GitHub.Repo))
extractFromConfig (Left _error) = (Nothing, Nothing)
extractFromConfig (Right config) = go (Nothing, Nothing) config
    where
        go :: (Maybe (GitHub.Name GitHub.Owner), Maybe (GitHub.Name GitHub.Repo))
           -> GitConfig
           -> (Maybe (GitHub.Name GitHub.Owner), Maybe (GitHub.Name GitHub.Repo))
        go acc [] = acc
        go acc (Section ("remote":_remoteName) entries:t) = case HashMap.lookup "url" entries of
            (Just url) -> error $ show url -- TODO
            Nothing -> go acc t
        go acc (_h:t) = go acc t

        -- Section ["remote","origin"] (fromList [("fetch","+refs/heads/*:refs/remotes/origin/*"),("url","git@github.com:spamviech/Zugkontrolle.git")])
main :: IO ()
main = do
    let gitConfig = "../.git/config"
    (localOwner, localRepository) <- doesFileExist gitConfig >>= \case
        True -> extractFromConfig . parseConfig <$> Text.readFile gitConfig
        False -> pure (Nothing, Nothing)
    Repository {owner, repository, directory} <- execParser
        $ info (helper <*> repositoryOption localOwner localRepository)
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
