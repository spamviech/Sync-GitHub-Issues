{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Applicative (Alternative((<|>)))
import qualified Control.Exception as Exception
import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import qualified Data.Attoparsec.Combinator as Attoparsec
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bifunctor (Bifunctor(bimap, first))
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.List (foldl', partition)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import ExitCodes (ExitCode(..), exitWith)
-- import qualified Data.ByteString as ByteString
import GitHub (github, Issue(..), IssueComment(..)  {-, NewIssue(..), IssueNumber(..)-})
import qualified GitHub
import Repository (Repository(..), parseRepositoryInformation)
import System.IO (Handle, utf8, hSetEncoding, hSetNewlineMode, stderr, hPutStrLn, NewlineMode(..)
                , Newline(..), hPrint, withFile, IOMode(ReadMode, WriteMode))

import Debug.Trace

-- | Like 'withFile', but set encoding to 'utf8'.
-- Newlines are converted to unix-style on input, unchanged (i.e. unix-style) on output.
withFileUtf8 :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileUtf8 filePath ioMode f = withFile filePath ioMode $ \handle -> do
    hSetEncoding handle utf8
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = LF }
    f handle

data LocalIssue =
    LocalIssue { title :: Text, body :: Maybe Text, comments :: HashMap Int LocalComment }
    deriving (Show, Eq)

newtype LocalComment = LocalComment { comment :: Text }
    deriving (Show, Eq)

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

{-
Format:
- Eine Datei "Issues.txt" (macht nur Sinn bei überschaubarer Issue-Anzahl)
- Erst offene Issues bis Trennzeile (_______________________________), danach geschlossene Issues
- Issue-Format:
    #Issue: (<Issue-Number>|new)
    <Titel, einzeilig>
    [<Leerzeile>
    <Body, mehrzeilig>]
    (~~~~~~~~~~~
    #Comment: (<Comment-Number>|new)
    <Comment, mehrzeilig>)*
    -------------------------
    #Issue: (<Issue-Number>|new)
    ...
-}
parseIssues
    :: FilePath
    -> ExceptT
        String
        IO
        (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
parseIssues filePath =
    ExceptT
    $ Exception.handle (\(_e :: Exception.IOException) -> pure $ Right (HashMap.empty, []))
    $ Attoparsec.eitherResult
    . signalEndOfInput
    . Attoparsec.parse (parseContents <* Attoparsec.endOfInput)
    <$> withFileUtf8 filePath ReadMode Text.hGetContents
    where
        signalEndOfInput :: Attoparsec.Result a -> Attoparsec.Result a
        signalEndOfInput (Attoparsec.Partial f) = f Text.empty
        signalEndOfInput fullResult = fullResult

        splitKnownNew :: (Hashable k, Eq k) => [(Maybe k, v)] -> (HashMap k v, [v])
        splitKnownNew =
            bimap (HashMap.fromList . map (first fromJust)) (map snd) . partition (isJust . fst)

        -- TODO parse closed issues
        parseContents :: Attoparsec.Parser
                          ( HashMap GitHub.IssueNumber (LocalIssue, [LocalComment])
                          , [(LocalIssue, [LocalComment])]
                          )
        parseContents = splitKnownNew <$> parseManyIssues []

        parseManyIssues
            :: [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
            -> Attoparsec.Parser [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
        parseManyIssues acc =
            (parseManyIssues . (: acc) =<< parseIssue parseIssueSep)
            <|> fmap (: acc) (parseIssue $ parseOpenClosedSep <|> Attoparsec.endOfInput)

        parseIssue :: Attoparsec.Parser ()
                   -> Attoparsec.Parser (Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))
        parseIssue parseEndOfIssue = do
            issueNumber <- Attoparsec.string issueHeader
                *> (fmap GitHub.IssueNumber <$> parseIdOrNew)
            Attoparsec.endOfLine
            title <- Attoparsec.takeTill Attoparsec.isEndOfLine
            Attoparsec.choice
                [ ( issueNumber
                  , (LocalIssue { title, body = Nothing, comments = HashMap.empty }, [])
                  )
                  <$ parseEndOfIssue
                , (issueNumber, )
                  <$> (first <$> (LocalIssue title . Just <$> parseBody parseEndOfIssue)
                       <*> parseComments parseEndOfIssue)
                  <* parseEndOfIssue
                , (issueNumber, ) . first (LocalIssue title Nothing)
                  <$> parseComments parseEndOfIssue
                  <* parseEndOfIssue]

        parseTillNextCommentOrIssue :: Attoparsec.Parser () -> Attoparsec.Parser Text
        parseTillNextCommentOrIssue parseEndOfIssue =
            Text.pack
            <$> Attoparsec.manyTill'
                Attoparsec.anyChar
                (Attoparsec.lookAhead $ parseCommentSep <|> parseEndOfIssue)

        parseBody :: Attoparsec.Parser () -> Attoparsec.Parser Text
        parseBody parseEndOfIssue = do
            _newlines <- Attoparsec.count 2 Attoparsec.endOfLine
            parseTillNextCommentOrIssue parseEndOfIssue

        parseComments
            :: Attoparsec.Parser () -> Attoparsec.Parser (HashMap Int LocalComment, [LocalComment])
        parseComments parseEndOfIssue = splitKnownNew <$> Attoparsec.many' parseComment
            where
                parseComment :: Attoparsec.Parser (Maybe Int, LocalComment)
                parseComment = do
                    parseCommentSep
                    commentNumber <- Attoparsec.string commentHeader *> parseIdOrNew
                    (commentNumber, ) . LocalComment
                        <$> parseTillNextCommentOrIssue parseEndOfIssue

        parseIdOrNew :: Attoparsec.Parser (Maybe Int)
        parseIdOrNew = (Just <$> Attoparsec.decimal) <|> (Nothing <$ Attoparsec.string "new")

        parseSepLine :: Char -> Attoparsec.Parser ()
        parseSepLine sepChar = do
            Attoparsec.endOfLine
            Attoparsec.skipMany1 $ Attoparsec.char sepChar
            Attoparsec.endOfLine

        commentSep :: Char
        commentSep = '~'

        parseCommentSep :: Attoparsec.Parser ()
        parseCommentSep = parseSepLine commentSep

        issueSep :: Char
        issueSep = '-'

        parseIssueSep :: Attoparsec.Parser ()
        parseIssueSep = parseSepLine issueSep

        openClosedSep :: Char
        openClosedSep = '_'

        parseOpenClosedSep :: Attoparsec.Parser ()
        parseOpenClosedSep = parseSepLine openClosedSep

issueHeader :: Text
issueHeader = "#Issue: "

commentHeader :: Text
commentHeader = "#Comment: "

showText :: (Show a) => a -> Text
showText = Text.pack . show

issuesToText
    :: (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue) -> Text
issuesToText (openIssues, closedIssues) =
    hashmapToText openIssues <> sepLine "__" <> hashmapToText closedIssues
    where
        sepLine :: Text -> Text
        sepLine c = Text.replicate 15 c <> "\n"

        hashmapToText :: HashMap GitHub.IssueNumber LocalIssue -> Text
        hashmapToText = foldl' prependIssue Text.empty . HashMap.toList

        prependIssue :: Text -> (GitHub.IssueNumber, LocalIssue) -> Text
        prependIssue acc issue
            | Text.null acc = issueToText issue     -- last issue doesn't get a separator
            | otherwise = issueToText issue <> sepLine "-" <> acc

        issueToText :: (GitHub.IssueNumber, LocalIssue) -> Text
        issueToText (GitHub.IssueNumber n, LocalIssue {title, body, comments}) =
            issueHeader
            <> showText n
            <> "\n"
            <> title
            <> "\n"
            <> maybe Text.empty (("\n" <>) . (<> "\n")) body
            <> mconcat (map commentToText $ HashMap.toList comments)

        commentToText :: (Int, LocalComment) -> Text
        commentToText (m, LocalComment {comment}) =
            sepLine "~" <> commentHeader <> showText m <> "\n" <> comment <> "\n"

main :: IO ()
main = do
    let tokenPath = ".githubtoken"
        tokenMissing = "No Token found in \"" ++ tokenPath ++ "\"."
    aut <- Exception.handle
        (\(_e :: Exception.IOException) -> hPutStrLn stderr tokenMissing >> exitWith NoTokenError)
        $ GitHub.OAuth <$> ByteString.readFile tokenPath
    Repository {owner, repository, filePath} <- parseRepositoryInformation
    putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    -- remoteIssues <- runExceptT (queryIssues aut owner repository) >>= \case
    --     Left err -> do
    --         hPrint stderr err
    --         exitWith ConnectionError
    --     Right issues -> pure issues
    localIssues <- runExceptT (parseIssues filePath) >>= \case
        Left err -> do
            hPutStrLn stderr err
            exitWith ParseFileError
        Right issues -> pure issues
    print localIssues
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
    -- let syncedIssues = (HashMap.map fst $ fst localIssues, HashMap.empty)
    -- let writeFailure = "Failed to write to \"" ++ filePath ++ "\""
    -- Exception.handle
    --     (\(_e :: Exception.IOException) -> hPutStrLn stderr writeFailure >> exitWith WriteException)
    --     $ withFileUtf8 filePath WriteMode
    --     $ flip Text.hPutStr
    --     $ issuesToText syncedIssues
    exitWith Success
