{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Repository (Repository(..), parseRepositoryInformation) where

import Control.Applicative (Alternative((<|>)))
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import qualified GitHub
import qualified GitHub.Data.Name as GitHub
import qualified Options.Applicative as Options
import Options.Applicative (help, long, metavar, progDesc, short, showDefault, strOption, value)
import System.Directory (doesFileExist)
import Text.GitConfig.Parser (parseConfig, Section(..), GitConfig)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

data Repository =
    Repository
    { owner :: GitHub.Name GitHub.Owner
    , repository :: GitHub.Name GitHub.Repo
    , directory :: FilePath
    }
    deriving (Show, Eq)

repositoryOption
    :: Maybe (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo) -> Options.Parser Repository
repositoryOption maybeOwnerRepository =
    Repository <$> ownerOption (fst <$> maybeOwnerRepository)
    <*> nameOption (snd <$> maybeOwnerRepository)
    <*> directoryOption

ownerOption :: Maybe (GitHub.Name GitHub.Owner) -> Options.Parser (GitHub.Name GitHub.Owner)
ownerOption remoteOwner =
    strOption
    $ long "owner"
    <> short 'o'
    <> metavar "OWNER"
    <> help "owner of the repository"
    <> maybe mempty value remoteOwner

nameOption :: Maybe (GitHub.Name GitHub.Repo) -> Options.Parser (GitHub.Name GitHub.Repo)
nameOption remoteRepository =
    strOption
    $ long "repository"
    <> short 'r'
    <> metavar "REPOSITORY"
    <> help "name of the repository"
    <> maybe mempty value remoteRepository

directoryOption :: Options.Parser FilePath
directoryOption =
    strOption
    $ long "directory"
    <> short 'd'
    <> metavar "DIRECTORY"
    <> help "directory where to store the local copy"
    <> value "Issues"
    <> showDefault

parseUrl :: Megaparsec.Parsec Void Text (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)
parseUrl = do
    -- Section ["remote","origin"] (fromList [("fetch","+refs/heads/*:refs/remotes/origin/*"),("url","git@github.com:spamviech/Zugkontrolle.git")])
    -- url = https://github.com/spamviech/Sync-GitHub-Issues.git
    _prefix <- Megaparsec.string "git@github.com:" <|> Megaparsec.string "https://github.com/"
    owner <- fmap (GitHub.N . Text.pack)
        $ Megaparsec.many
        $ Megaparsec.alphaNumChar <|> Megaparsec.char '-' <|> Megaparsec.char '_'
    _slash <- Megaparsec.char '/'
    repo <- fmap (GitHub.N . Text.pack)
        $ Megaparsec.many
        $ Megaparsec.alphaNumChar <|> Megaparsec.char '-' <|> Megaparsec.char '_'
    _git <- Megaparsec.string ".git"
    Megaparsec.eof
    pure (owner, repo)

extractFromConfig
    :: Either e GitConfig -> Maybe (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)
extractFromConfig (Left _error) = Nothing
extractFromConfig (Right config) = go config
    where
        go :: GitConfig -> Maybe (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)
        go [] = Nothing
        go (Section ("remote":_remoteName) entries:t) = case HashMap.lookup "url" entries of
            (Just url) -> Megaparsec.parseMaybe parseUrl url
            Nothing -> go t
        go (_h:t) = go t

readFromConfig :: Maybe (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)
               -> FilePath
               -> IO (Maybe (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo))
readFromConfig acc@(Just _result) _gitConfig = pure acc
readFromConfig Nothing gitConfig = doesFileExist gitConfig >>= \case
    True -> extractFromConfig . parseConfig <$> Text.readFile gitConfig
    False -> pure Nothing

parseRepositoryInformation :: IO Repository
parseRepositoryInformation = do
    maybeOwnerRepository <- foldM readFromConfig Nothing gitConfigs
    Options.execParser
        $ Options.info (Options.helper <*> repositoryOption maybeOwnerRepository)
        $ progDesc "Sync issues of the specified github.com repository with a local copy."
    where
        gitConfigs :: [FilePath]
        gitConfigs = [".git/config", "../.git/config"]
