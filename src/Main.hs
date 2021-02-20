{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(), runExceptT, withExceptT, throwE)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import ExitCodes (ExitCode(..), exitWith)
import qualified GitHub
import LocalCopy (readIssues, writeIssues, handleExceptT, showText)
import Repository (Repository(..), parseRepositoryInformation)
import SyncGitHub (queryIssues, calculateChanges, applyRemoteChanges)
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
    remoteIssues <- withExceptT ((ConnectionError, ) . showText)
        $ queryIssues aut owner repository
        $ Map.keys
        $ fst localIssues
    syncedIssues <- withExceptT ((ConnectionError, ) . showText)
        $ applyRemoteChanges aut owner repository
        $ calculateChanges remoteIssues localIssues
    withExceptT (ParseFileError, ) $ writeIssues filePath syncedIssues
