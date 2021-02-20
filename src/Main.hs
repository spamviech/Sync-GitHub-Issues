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
import Lens.Micro (_5)
import Lens.Micro.Extras (view)
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
    remoteIssues <- if queryGitHub
        then withExceptT ((ConnectionError, ) . showText)
            $ queryIssues aut owner repository
            $ Map.keys (fst $ fst localIssues) ++ Map.keys (fst $ snd localIssues)
        else pure (Map.empty, Map.empty)
    syncedIssues <- if updateGitHub
        then withExceptT ((ConnectionError, ) . showText)
            $ applyRemoteChanges aut
            $ calculateChanges remoteIssues localIssues
        else pure $ view _5 $ calculateChanges remoteIssues localIssues
    if writeToFile
        then withExceptT (ParseFileError, ) $ writeIssues filePath syncedIssues
        else lift $ print syncedIssues
    where
        -- TODO Dev-Flags so file-overwrites/api-access can be restricted
        queryGitHub = False

        updateGitHub = True

        writeToFile = False
