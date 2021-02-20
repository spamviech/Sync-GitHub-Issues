{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module LocalCopy
  ( -- * Data types
    LocalIssue(..)
  , LocalComment(..)
    -- * Functions
  , readIssues
  , parseIssues
  , issuesToText
  , writeIssues
    -- ** Utility functions
  , handleExceptT
  , showText
  ) where

import Control.Applicative (Alternative((<|>)))
import qualified Control.Exception as Exception
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import qualified Data.Attoparsec.Combinator as Attoparsec
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Bifunctor (Bifunctor(bimap, first))
import Data.List (foldl', partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime())
import qualified GitHub
import System.Directory (getModificationTime)
import System.IO (Handle, utf8, hSetEncoding, hSetNewlineMode, NewlineMode(..), Newline(..)
                , withFile, IOMode(ReadMode, WriteMode))

-- | Like 'withFile', but set encoding to 'utf8'.
-- Newlines are converted to unix-style on input, unchanged (i.e. unix-style) on output.
withFileUtf8 :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileUtf8 filePath ioMode f = withFile filePath ioMode $ \handle -> do
    hSetEncoding handle utf8
    hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = LF }
    f handle

data LocalIssue =
    LocalIssue
    { title :: Text
    , modificationTime :: UTCTime
    , body :: Maybe Text
    , comments :: Map Int LocalComment
    }
    deriving (Show, Eq)

data LocalComment = LocalComment { comment :: Text, modificationTime :: UTCTime }
    deriving (Show, Eq)

instance Semigroup LocalComment where
    (<>) :: LocalComment -> LocalComment -> LocalComment
    (<>) c0@LocalComment {modificationTime = t0} c1@LocalComment {modificationTime = t1}
        | t0 < t1 = c1
        | otherwise = c0

-- defined here for simplicity
-- maybe move to its own module, reexporting ExceptT and MonadTrans?
handleExceptT :: (Exception.IOException -> ExceptT e IO a) -> ExceptT e IO a -> ExceptT e IO a
handleExceptT handleWith action =
    ExceptT $ Exception.handle (runExceptT . handleWith) $ runExceptT action

readIssues
    :: FilePath
    -> ExceptT
        String
        IO
        ( (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        , (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        )
readIssues filePath = handleExceptT (\_e -> pure ((Map.empty, []), (Map.empty, []))) $ do
    modificationTime <- lift $ getModificationTime filePath
    ExceptT
        $ Attoparsec.eitherResult
        . signalEndOfInput
        . Attoparsec.parse (parseIssues modificationTime)
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
    :: UTCTime
    -> Attoparsec.Parser
        ( (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        , (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
        )
parseIssues
    modificationTime = (,) <$> parseOpenIssues <*> parseClosedIssues <* Attoparsec.endOfInput
    where
        parseOpenIssues :: Attoparsec.Parser
                            ( Map GitHub.IssueNumber (LocalIssue, [LocalComment])
                            , [(LocalIssue, [LocalComment])]
                            )
        parseOpenIssues =
            splitKnownNew <$> parseManyIssues (parseOpenClosedSep <|> Attoparsec.endOfInput) []

        parseClosedIssues :: Attoparsec.Parser
                              ( Map GitHub.IssueNumber (LocalIssue, [LocalComment])
                              , [(LocalIssue, [LocalComment])]
                              )
        parseClosedIssues =
            ((Map.empty, []) <$ Attoparsec.endOfInput)
            <|> (parseOpenClosedSep *> (splitKnownNew <$> parseManyIssues Attoparsec.endOfInput []))

        splitKnownNew :: (Ord k, Eq k) => [(Maybe k, v)] -> (Map k v, [v])
        splitKnownNew =
            bimap (Map.fromList . map (first fromJust)) (map snd) . partition (isJust . fst)

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
                  , ( LocalIssue { title, modificationTime, body = Nothing, comments = Map.empty }
                    , []
                    )
                  )
                  <$ parseEndOfIssue
                , (issueNumber, )
                  <$> (first
                       <$> (LocalIssue title modificationTime . Just <$> parseBody parseEndOfIssue)
                       <*> parseComments parseEndOfIssue)
                  <* parseEndOfIssue
                , (issueNumber, ) . first (LocalIssue title modificationTime Nothing)
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
            :: Attoparsec.Parser () -> Attoparsec.Parser (Map Int LocalComment, [LocalComment])
        parseComments parseEndOfIssue = splitKnownNew <$> Attoparsec.many' parseComment
            where
                parseComment :: Attoparsec.Parser (Maybe Int, LocalComment)
                parseComment = do
                    parseCommentSep
                    commentNumber <- Attoparsec.string commentHeader *> parseIdOrNew
                    (commentNumber, ) . flip LocalComment modificationTime
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

issuesToText :: (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue) -> Text
issuesToText
    (openIssues, closedIssues) = mapToText openIssues <> sepLine "__" <> mapToText closedIssues
    where
        sepLine :: Text -> Text
        sepLine c = Text.replicate 15 c <> "\n"

        mapToText :: Map GitHub.IssueNumber LocalIssue -> Text
        mapToText = foldl' prependIssue Text.empty . Map.toAscList

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
            <> mconcat (map commentToText $ Map.toAscList comments)

        commentToText :: (Int, LocalComment) -> Text
        commentToText (m, LocalComment {comment}) =
            sepLine "~" <> commentHeader <> showText m <> "\n" <> comment <> "\n"

writeIssues :: FilePath
            -> (Map GitHub.IssueNumber LocalIssue, Map GitHub.IssueNumber LocalIssue)
            -> ExceptT Text IO ()
writeIssues filePath syncedIssues =
    handleExceptT (\(_e :: Exception.IOException) -> throwE errorMessage)
    $ lift
    $ withFileUtf8 filePath WriteMode
    $ flip Text.hPutStr fileContents
    where
        fileContents :: Text
        fileContents = issuesToText syncedIssues

        errorMessage :: Text
        errorMessage = "Failed to write to \"" <> Text.pack filePath <> "\"!\n" <> fileContents
