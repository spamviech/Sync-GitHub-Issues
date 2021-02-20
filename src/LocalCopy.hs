{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

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
import qualified GitHub.Data.Id as GitHub
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
    , state :: GitHub.IssueState
    , modificationTime :: UTCTime
    , body :: Maybe Text
    , comments :: Map (GitHub.Id GitHub.Comment) LocalComment
    }
    deriving (Show, Eq)

-- | Take the newer Issue, merging comments using their Semigroup instance.
-- At equal modification time, the left one wins.
instance Semigroup LocalIssue where
    (<>) :: LocalIssue -> LocalIssue -> LocalIssue
    (<>)
        i0@LocalIssue {modificationTime = t0, comments = c0, state = s0}
        i1@LocalIssue {modificationTime = t1, comments = c1, state = s1}
        | t0 < t1 = i1 { comments, state = s1 }
        | otherwise = i0 { comments, state = s0 }
        where
            comments :: Map (GitHub.Id GitHub.Comment) LocalComment
            comments = Map.unionWith (<>) c0 c1

data LocalComment = LocalComment { comment :: Text, modificationTime :: UTCTime }
    deriving (Show, Eq)

-- | Take the newer Comment. At equal modification time, the left one wins.
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
        (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
readIssues filePath = handleExceptT (\_e -> pure (Map.empty, [])) $ do
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
        (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
parseIssues
    modificationTime = (<>) <$> parseOpenIssues <*> parseClosedIssues <* Attoparsec.endOfInput
    where
        parseOpenIssues :: Attoparsec.Parser
                            ( Map GitHub.IssueNumber (LocalIssue, [LocalComment])
                            , [(LocalIssue, [LocalComment])]
                            )
        parseOpenIssues = splitKnownNew <$> parseManyIssues GitHub.StateOpen []

        parseClosedIssues :: Attoparsec.Parser
                              ( Map GitHub.IssueNumber (LocalIssue, [LocalComment])
                              , [(LocalIssue, [LocalComment])]
                              )
        parseClosedIssues =
            ((Map.empty, []) <$ Attoparsec.endOfInput)
            <|> (parseOpenClosedSep *> (splitKnownNew <$> parseManyIssues GitHub.StateClosed []))

        splitKnownNew :: (Ord k, Eq k) => [(Maybe k, v)] -> (Map k v, [v])
        splitKnownNew =
            bimap (Map.fromList . map (first fromJust)) (map snd) . partition (isJust . fst)

        parseManyIssues
            :: GitHub.IssueState
            -> [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
            -> Attoparsec.Parser [(Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))]
        parseManyIssues state acc =
            ([] <$ Attoparsec.endOfInput)
            <|> (parseIssue state parseIssueSep >>= parseManyIssues state . (: acc))
            <|> fmap (: acc) (parseIssue state $ Attoparsec.lookAhead parseEndSep)
            where
                parseEndSep :: Attoparsec.Parser ()
                parseEndSep = case state of
                    GitHub.StateOpen -> parseOpenClosedSep <|> Attoparsec.endOfInput
                    GitHub.StateClosed -> Attoparsec.endOfInput

        parseIssue :: GitHub.IssueState
                   -> Attoparsec.Parser ()
                   -> Attoparsec.Parser (Maybe GitHub.IssueNumber, (LocalIssue, [LocalComment]))
        parseIssue state parseEndOfIssue = do
            issueNumber <- Attoparsec.string issueHeader
                *> (fmap GitHub.IssueNumber <$> parseIdOrNew)
            Attoparsec.endOfLine
            title <- Attoparsec.takeTill Attoparsec.isEndOfLine
            Attoparsec.choice
                [ ( issueNumber
                  , ( LocalIssue
                      { title, state, modificationTime, body = Nothing, comments = Map.empty }
                    , []
                    )
                  )
                  <$ parseEndOfIssue
                , (issueNumber, )
                  <$> (first
                       <$> (LocalIssue title state modificationTime . Just
                            <$> parseBody parseEndOfIssue)
                       <*> parseComments parseEndOfIssue)
                  <* parseEndOfIssue
                , (issueNumber, ) . first (LocalIssue title state modificationTime Nothing)
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
            :: Attoparsec.Parser ()
            -> Attoparsec.Parser (Map (GitHub.Id GitHub.Comment) LocalComment, [LocalComment])
        parseComments parseEndOfIssue = splitKnownNew <$> Attoparsec.many' parseComment
            where
                parseComment :: Attoparsec.Parser (Maybe (GitHub.Id GitHub.Comment), LocalComment)
                parseComment = do
                    parseCommentSep
                    commentNumber <- Attoparsec.string commentHeader *> parseIdOrNew
                    Attoparsec.endOfLine
                    (GitHub.Id <$> commentNumber, ) . flip LocalComment modificationTime
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

issuesToText :: Map GitHub.IssueNumber LocalIssue -> Text
issuesToText (Map.partition ((== GitHub.StateOpen) . state) -> (openIssues, closedIssues)) =
    mapToText openIssues <> sepLine "__" <> mapToText closedIssues
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

        commentToText :: (GitHub.Id GitHub.Comment, LocalComment) -> Text
        commentToText (GitHub.Id m, LocalComment {comment}) =
            sepLine "~" <> commentHeader <> showText m <> "\n" <> comment <> "\n"

writeIssues :: FilePath -> Map GitHub.IssueNumber LocalIssue -> ExceptT Text IO ()
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
