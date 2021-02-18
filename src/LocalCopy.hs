{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LocalCopy
  ( LocalIssue(..)
  , LocalComment(..)
  , readIssues
  , parseIssues
  , issuesToText
  , writeIssues
  ) where

import Control.Applicative (Alternative((<|>)))
import qualified Control.Exception as Exception
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import qualified Data.Attoparsec.Combinator as Attoparsec
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bifunctor (Bifunctor(bimap, first))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.List (foldl', partition)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import ExitCodes (ExitCode(..), exitWith)
import qualified GitHub
import System.IO (Handle, utf8, hSetEncoding, hSetNewlineMode, stderr, hPutStrLn, NewlineMode(..)
                , Newline(..), withFile, IOMode(ReadMode, WriteMode))

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

readIssues
    :: FilePath
    -> ExceptT
        String
        IO
        ( (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        , (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        )
readIssues filePath =
    ExceptT
    $ Exception.handle
        (\(_e :: Exception.IOException) -> pure $ Right ((HashMap.empty, []), (HashMap.empty, [])))
    $ Attoparsec.eitherResult . signalEndOfInput . Attoparsec.parse parseIssues
    <$> withFileUtf8 filePath ReadMode Text.hGetContents
    where
        signalEndOfInput :: Attoparsec.Result a -> Attoparsec.Result a
        signalEndOfInput (Attoparsec.Partial f) = f Text.empty
        signalEndOfInput fullResult = fullResult

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
    :: Attoparsec.Parser
        ( (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        , (HashMap GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        )
parseIssues = (,) <$> parseOpenIssues <*> parseClosedIssues <* Attoparsec.endOfInput
    where
        parseOpenIssues :: Attoparsec.Parser
                            ( HashMap GitHub.IssueNumber (LocalIssue, [LocalComment])
                            , [(LocalIssue, [LocalComment])]
                            )
        parseOpenIssues =
            splitKnownNew <$> parseManyIssues (parseOpenClosedSep <|> Attoparsec.endOfInput) []

        parseClosedIssues :: Attoparsec.Parser
                              ( HashMap GitHub.IssueNumber (LocalIssue, [LocalComment])
                              , [(LocalIssue, [LocalComment])]
                              )
        parseClosedIssues =
            ((HashMap.empty, []) <$ Attoparsec.endOfInput)
            <|> (parseOpenClosedSep *> (splitKnownNew <$> parseManyIssues Attoparsec.endOfInput []))

        splitKnownNew :: (Hashable k, Eq k) => [(Maybe k, v)] -> (HashMap k v, [v])
        splitKnownNew =
            bimap (HashMap.fromList . map (first fromJust)) (map snd) . partition (isJust . fst)

        parseManyIssues
            :: Attoparsec.Parser ()
            -> [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
            -> Attoparsec.Parser [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
        parseManyIssues parseEndSep acc =
            (parseManyIssues parseEndSep . (: acc) =<< parseIssue parseIssueSep)
            <|> fmap (: acc) (parseIssue $ Attoparsec.lookAhead parseEndSep)

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

writeIssues :: FilePath
            -> (HashMap GitHub.IssueNumber LocalIssue, HashMap GitHub.IssueNumber LocalIssue)
            -> IO ()
writeIssues filePath syncedIssues =
    Exception.handle
        (\(_e :: Exception.IOException) -> showErrorMessage >> exitWith WriteException)
    $ withFileUtf8 filePath WriteMode
    $ flip Text.hPutStr fileContents
    where
        fileContents :: Text
        fileContents = issuesToText syncedIssues

        showErrorMessage :: IO ()
        showErrorMessage = do
            hPutStrLn stderr $ "Failed to write to \"" ++ filePath ++ "\"!"
            print fileContents
