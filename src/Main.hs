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
import Data.List (partition)
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
import System.IO (Handle, utf8, hSetEncoding, hSetNewlineMode, stderr, hPutStrLn
                , noNewlineTranslation, hPrint, withFile, IOMode(ReadMode, WriteMode))

-- | Like 'withFile', but set encoding to 'utf8' with 'noNewlineTranslation'.
withFileUtf8 :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileUtf8 filePath ioMode f = withFile filePath ioMode $ \handle -> do
    hSetEncoding handle utf8
    hSetNewlineMode handle noNewlineTranslation
    f handle

data LocalIssue =
    LocalIssue
    { title :: Text
    , body :: Maybe Text
    , comments :: HashMap Int LocalComment
    , newComments :: [LocalComment]
    }
    deriving (Show, Eq)

newtype LocalComment = LocalComment { comment :: Text }
    deriving (Show, Eq)

queryIssues :: (GitHub.AuthMethod auth)
            => auth
            -> GitHub.Name GitHub.Owner
            -> GitHub.Name GitHub.Repo
            -> ExceptT GitHub.Error IO (HashMap GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository = do
    issues <- ExceptT $ github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    fmap HashMap.fromList
        $ forM (Vector.toList issues)
        $ \Issue {issueNumber, issueTitle, issueBody} -> do
            -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
            let idBodyTuple c = (issueCommentId c, LocalComment { comment = issueCommentBody c })
            comments <- fmap (HashMap.fromList . map idBodyTuple . Vector.toList)
                $ ExceptT
                $ github aut (GitHub.commentsR owner repository issueNumber GitHub.FetchAll)
            pure
                ( issueNumber
                , LocalIssue { title = issueTitle, body = issueBody, comments, newComments = [] }
                )

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
parseIssues :: FilePath -> ExceptT String IO (HashMap GitHub.IssueNumber LocalIssue, [LocalIssue])
parseIssues filePath =
    ExceptT
    $ Exception.handle (\(_e :: Exception.IOException) -> pure $ Right (HashMap.empty, []))
    $ Attoparsec.eitherResult . Attoparsec.parse parseContents
    <$> withFileUtf8 filePath ReadMode Text.hGetContents
    where
        splitKnownNew :: (Hashable k, Eq k) => [(Maybe k, v)] -> (HashMap k v, [v])
        splitKnownNew =
            bimap (HashMap.fromList . map (first fromJust)) (map snd) . partition (isJust . fst)

        parseContents :: Attoparsec.Parser (HashMap GitHub.IssueNumber LocalIssue, [LocalIssue])
        parseContents = fmap splitKnownNew $ Attoparsec.many' $ do
            issueNumber <- Attoparsec.string issueHeader
                *> (fmap GitHub.IssueNumber <$> parseIdOrNew)
            Attoparsec.endOfLine
            title <- Attoparsec.takeTill Attoparsec.isEndOfLine
            Attoparsec.choice
                [ ( issueNumber
                  , LocalIssue
                    { title, body = Nothing, comments = HashMap.empty, newComments = [] }
                  )
                  <$ parseEndOfIssue
                , (issueNumber, )
                  <$> (uncurry <$> (LocalIssue title . Just <$> parseBody) <*> parseComments)
                , (issueNumber, ) . uncurry (LocalIssue title Nothing) <$> parseComments]

        parseBody :: Attoparsec.Parser Text
        parseBody = do
            _newlines <- Attoparsec.count 2 Attoparsec.endOfLine
            head
                <$> Attoparsec.manyTill'
                    (Attoparsec.takeWhile $ const True)
                    (Attoparsec.lookAhead parseCommentSep <|> parseEndOfIssue)

        parseComments :: Attoparsec.Parser (HashMap Int LocalComment, [LocalComment])
        parseComments = do
            fmap splitKnownNew $ Attoparsec.many' $ do
                parseCommentSep
                commentNumber <- Attoparsec.string commentHeader *> parseIdOrNew
                (commentNumber, ) . LocalComment . head
                    <$> Attoparsec.manyTill'
                        (Attoparsec.takeWhile $ const True)
                        (Attoparsec.lookAhead parseCommentSep <|> parseEndOfIssue)

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

        parseEndOfIssue :: Attoparsec.Parser ()
        parseEndOfIssue = parseIssueSep <|> Attoparsec.endOfInput

issueHeader :: Text
issueHeader = "#Issue: "

commentHeader :: Text
commentHeader = "#Comment: "

showText :: (Show a) => a -> Text
showText = Text.pack . show

issuesToText :: HashMap GitHub.IssueNumber LocalIssue -> Text
issuesToText = mconcat . map concatIssue . HashMap.toList
    where
        concatIssue :: (GitHub.IssueNumber, LocalIssue) -> Text
        concatIssue (GitHub.IssueNumber n, LocalIssue {title, body, comments, newComments}) =
            issueHeader
            <> showText n
            <> "\n"
            <> title
            <> "\n"
            <> maybe Text.empty (("\n" <>) . (<> "\n")) body
            <> mconcat (map knownCommentToText $ HashMap.toList comments)
            <> mconcat (map newCommentToText newComments)
            <> "-----------\n"

        knownCommentToText :: (Int, LocalComment) -> Text
        knownCommentToText (m, LocalComment {comment}) =
            "~~~~~~~~~\n" <> commentHeader <> showText m <> "\n" <> comment <> "\n"

        newCommentToText :: LocalComment -> Text
        newCommentToText
            LocalComment {comment} = "-----------\n" <> commentHeader <> "new\n" <> comment <> "\n"

main :: IO ()
main = do
    let tokenPath = ".githubtoken"
        tokenMissing = "No Token found in \"" ++ tokenPath ++ "\"."
    aut <- Exception.handle
        (\(_e :: Exception.IOException) -> hPutStrLn stderr tokenMissing >> exitWith NoTokenError)
        $ GitHub.OAuth <$> ByteString.readFile tokenPath
    Repository {owner, repository, filePath} <- parseRepositoryInformation
    putStrLn $ "owner: " ++ show owner ++ ", repository: " ++ show repository
    issueFileContents <- Exception.handle (\(_e :: Exception.IOException) -> pure Text.empty)
        $ withFileUtf8 filePath ReadMode Text.hGetContents
    print issueFileContents
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    remoteIssues <- runExceptT (queryIssues aut owner repository) >>= \case
        Left err -> do
            hPrint stderr err
            exitWith ConnectionError
        Right issues -> pure issues
    localIssues <- runExceptT (parseIssues filePath) >>= \case
        Left err -> do
            hPutStrLn stderr err
            exitWith ConnectionError
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
    Exception.handle (\(_e :: Exception.IOException) -> pure ())
        $ withFileUtf8 filePath WriteMode
        $ flip Text.hPutStr
        $ issuesToText remoteIssues
    exitWith Success
