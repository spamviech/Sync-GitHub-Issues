{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module SyncGitHub (queryIssues, calculateChanges, applyRemoteChanges) where

import Control.Monad (forM, foldM)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
-- import qualified Data.ByteString as ByteString
import GitHub (github, Issue(..), IssueComment(..), Comment(..), NewIssue(..), EditIssue(..))
import qualified GitHub
import qualified GitHub.Data.Id as GitHub
import qualified GitHub.Endpoints.Issues as GitHub
import LocalCopy (LocalIssue(..), LocalComment(..))

-- | Ask /github.com/ for all currently open issues.
queryIssues :: (GitHub.AuthMethod auth)
            => auth
            -> GitHub.Name GitHub.Owner
            -> GitHub.Name GitHub.Repo
            -> [GitHub.IssueNumber]
            -> ExceptT GitHub.Error IO (Map GitHub.IssueNumber LocalIssue)
queryIssues aut owner repository knownIssueNumbers = do
    -- issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
    issues <- ExceptT $ github aut $ GitHub.issuesForRepoR owner repository mempty GitHub.FetchAll
    openIssues <- Map.fromList <$> forM (Vector.toList issues) localIssue
    foldM lookupIssue openIssues knownIssueNumbers
    where
        lfNewlines :: Text -> Text
        lfNewlines = Text.replace "\r\n" "\n"

        localIssue :: GitHub.Issue -> ExceptT GitHub.Error IO (GitHub.IssueNumber, LocalIssue)
        localIssue Issue {issueNumber, issueTitle, issueState, issueBody, issueUpdatedAt} = do
            -- commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
            let idBodyTuple IssueComment {issueCommentId, issueCommentBody, issueCommentUpdatedAt} =
                    ( GitHub.Id issueCommentId
                    , LocalComment
                      { comment = lfNewlines issueCommentBody
                      , modificationTime = issueCommentUpdatedAt
                      }
                    )
            comments <- fmap (Map.fromList . map idBodyTuple . Vector.toList)
                $ ExceptT
                $ github aut (GitHub.commentsR owner repository issueNumber GitHub.FetchAll)
            pure
                ( issueNumber
                , LocalIssue
                  { title = lfNewlines issueTitle
                  , state = issueState
                  , modificationTime = issueUpdatedAt
                  , body = lfNewlines <$> issueBody
                  , comments
                  }
                )

        lookupIssue :: Map GitHub.IssueNumber LocalIssue
                    -> GitHub.IssueNumber
                    -> ExceptT GitHub.Error IO (Map GitHub.IssueNumber LocalIssue)
        lookupIssue acc issueNumber
            | Map.member issueNumber acc = pure acc
            | otherwise = do
                -- issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
                issue <- ExceptT (github aut $ GitHub.issueR owner repository issueNumber)
                flip (uncurry Map.insert) acc <$> localIssue issue

type ChangeInformation =
    ( [(GitHub.NewIssue, [Text], GitHub.IssueState)]            -- createIssueR, createCommentR, editIssueR
    , [(GitHub.IssueNumber, GitHub.EditIssue)]                  -- editIssueR
    , [(GitHub.IssueNumber, Text)]                              -- createCommentR
    , [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]    -- editCommentR
    , Map GitHub.IssueNumber LocalIssue                         -- unchangedIssues
    )

type RemoteLocalIssueState = (Maybe LocalIssue, Maybe LocalIssue)

-- | Calculate required changes to achieve a synced state.
calculateChanges
    :: Map GitHub.IssueNumber LocalIssue
    -> (Map GitHub.IssueNumber (LocalIssue, [LocalComment]), [(LocalIssue, [LocalComment])])
    -> ChangeInformation
calculateChanges remoteIssues (knownLocalIssues, newLocalIssues) =
    (newIssues, editIssues, newComments, editComments, unchangedIssues)
    where
        -- new Issue can't have known comments
        -- thus, they will be created as well, probably assigning a new ID in the process
        newIssues :: [(GitHub.NewIssue, [Text], GitHub.IssueState)]
        newIssues =
            [( NewIssue
               { newIssueTitle = title
               , newIssueBody = body
               , newIssueAssignees = Vector.empty
               , newIssueMilestone = Nothing
               , newIssueLabels = Nothing
               }
             , map comment $ Map.elems comments ++ newIssueComments
             , state
             ) | (LocalIssue {title, state, body, comments}, newIssueComments) <- newLocalIssues]

        newComments :: [(GitHub.IssueNumber, Text)]
        newComments =
            concat $ Map.elems $ Map.mapWithKey newCommentsWithIssueNumber knownLocalIssues
            where
                newCommentsWithIssueNumber
                    :: GitHub.IssueNumber -> (a, [LocalComment]) -> [(GitHub.IssueNumber, Text)]
                newCommentsWithIssueNumber issueNumber = map ((issueNumber, ) . comment) . snd

        editIssues :: [(GitHub.IssueNumber, GitHub.EditIssue)]
        editComments :: [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
        unchangedIssues :: (Map GitHub.IssueNumber LocalIssue)
        (editIssues, editComments, Map.fromList -> unchangedIssues) =
            foldl' syncIssue ([], [], [])
            $ Map.toAscList
            $ Map.unionWith
                (<>)
                (Map.map mapRemote remoteIssues)
                (Map.map mapLocal knownLocalIssues)

        mapRemote :: LocalIssue -> RemoteLocalIssueState
        mapRemote = (, Nothing) . Just

        mapLocal :: (LocalIssue, [LocalComment]) -> RemoteLocalIssueState
        mapLocal = (Nothing, ) . Just . fst

        syncIssue :: ( [(GitHub.IssueNumber, GitHub.EditIssue)]
                     , [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
                     , [(GitHub.IssueNumber, LocalIssue)]
                     )
                  -> (GitHub.IssueNumber, RemoteLocalIssueState)
                  -> ( [(GitHub.IssueNumber, GitHub.EditIssue)]
                     , [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
                     , [(GitHub.IssueNumber, LocalIssue)]
                     )
        syncIssue
            (editIssues', editComments', unchangedIssues')
            ( issueNumber
            , ( Just
                    remoteIssue@LocalIssue { state = remoteState
                                           , title = remoteTitle
                                           , modificationTime = remoteTime
                                           , body = remoteBody
                                           , comments = remoteComments}
              , Just
                    LocalIssue { state = localState
                               , title = localTitle
                               , modificationTime = localTime
                               , body = localBody
                               , comments = localComments}
              )
            )
            | localTime < remoteTime
                || (remoteState == localState
                    && remoteTitle == localTitle
                    && remoteBody == localBody) =
                ( editIssues'
                , editComments'' ++ editComments'
                , addUnchangedIssue issueNumber remoteIssue { comments } unchangedIssues'
                )
            | otherwise =
                ( (issueNumber, editIssue) : editIssues'
                , editComments'' ++ editComments'
                , unchangedIssues'
                )
            where
                editComments'' :: [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
                comments :: Map (GitHub.Id GitHub.Comment) LocalComment
                (editComments'', Map.fromList -> comments) =
                    foldl' syncComment ([], [])
                    $ Map.toAscList
                    $ Map.unionWith
                        (<>)
                        (Map.map ((, Nothing) . Just) remoteComments)
                        (Map.map ((Nothing, ) . Just) localComments)

                syncComment :: ( [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
                               , [(GitHub.Id GitHub.Comment, LocalComment)]
                               )
                            -> (GitHub.Id GitHub.Comment, (Maybe LocalComment, Maybe LocalComment))
                            -> ( [(GitHub.IssueNumber, GitHub.Id GitHub.Comment, Text)]
                               , [(GitHub.Id GitHub.Comment, LocalComment)]
                               )
                syncComment
                    acc
                    ( commentId
                    , ( Just
                            remoteComment@LocalComment
                            {comment = remoteText, modificationTime = remoteCommentTime}
                      , Just
                            LocalComment {comment = localText, modificationTime = localCommentTime}
                      )
                    )
                    | localCommentTime < remoteCommentTime || remoteText == localText =
                        second ((commentId, remoteComment) :) acc
                    | otherwise = first ((issueNumber, commentId, localText) :) acc
                syncComment acc (commentId, (Just remoteComment, Nothing)) =
                    second ((commentId, remoteComment) :) acc
                syncComment acc (_commentId, (Nothing, _localComment)) = acc  -- deleted comment?

                editIssue :: GitHub.EditIssue
                editIssue =
                    GitHub.editOfIssue
                    { editIssueTitle = Just localTitle
                    , editIssueBody = Just $ fromMaybe "" localBody
                    , editIssueAssignees = Nothing
                    , editIssueState = Just localState
                    }
        syncIssue
            (editIssues', editComments', unchangedIssues')
            (issueNumber, (Just issue, Nothing)) =
            (editIssues', editComments', addUnchangedIssue issueNumber issue unchangedIssues')
        syncIssue acc (_issueNumber, (Nothing, _localIssue)) = acc  -- deleted issue?

        addUnchangedIssue :: GitHub.IssueNumber
                          -> LocalIssue
                          -> [(GitHub.IssueNumber, LocalIssue)]
                          -> [(GitHub.IssueNumber, LocalIssue)]
        addUnchangedIssue issueNumber issue = ((issueNumber, issue) :)

toLocalIssue :: GitHub.Issue -> (GitHub.IssueNumber, LocalIssue)
toLocalIssue Issue {issueNumber, issueTitle, issueBody, issueUpdatedAt, issueState} =
    ( issueNumber
    , LocalIssue
      { title = issueTitle
      , body = issueBody
      , state = issueState
      , modificationTime = issueUpdatedAt
      , comments = Map.empty
      }
    )

toLocalComment :: GitHub.Comment -> (GitHub.Id GitHub.Comment, LocalComment)
toLocalComment Comment {commentId, commentBody, commentUpdatedAt} =
    (commentId, LocalComment { comment = commentBody, modificationTime = commentUpdatedAt })

-- | Apply changes from 'calculateChanges' to /github.com/ and return the updated 'LocalIssues's.
applyRemoteChanges :: (GitHub.AuthMethod auth)
                   => auth
                   -> GitHub.Name GitHub.Owner
                   -> GitHub.Name GitHub.Repo
                   -> ChangeInformation
                   -> ExceptT GitHub.Error IO (Map GitHub.IssueNumber LocalIssue)
applyRemoteChanges
    aut
    owner
    repository
    (newIssues, editIssues, newComments, editComments, unchangedIssues) = do
    -- createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request RW Issue
    (Map.fromList -> newLocalIssues, catMaybes -> newIssueEdits, concat -> newIssueComments)
        <- fmap unzip3 $ forM newIssues $ \(newIssue, newIssueComments, localState) -> do
            issue@Issue {issueNumber, issueState}
                <- ExceptT $ github aut $ GitHub.createIssueR owner repository newIssue
            pure
                ( toLocalIssue issue
                , if localState == issueState
                      then Nothing
                      else Just
                          (issueNumber, GitHub.editOfIssue { editIssueState = Just localState })
                , (issueNumber, ) <$> newIssueComments
                )
    -- editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request RW Issue
    editedLocalIssues <- fmap Map.fromList
        $ forM (newIssueEdits ++ editIssues)
        $ fmap toLocalIssue . ExceptT . github aut . uncurry (GitHub.editIssueR owner repository)
    -- createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request RW Comment
    newLocalComments <- fmap (Map.fromListWith (<>))
        $ forM (newIssueComments ++ newComments)
        $ \(issueNumber, text) -> fmap ((issueNumber, ) . uncurry Map.singleton . toLocalComment)
        $ ExceptT
        $ github aut
        $ GitHub.createCommentR owner repository issueNumber text
    -- editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request RW Comment
    editedLocalComments
        <- fmap (Map.fromListWith (<>)) $ forM editComments $ \(issueNumber, commentId, text)
        -> fmap ((issueNumber, ) . uncurry Map.singleton . toLocalComment)
        $ ExceptT
        $ github aut
        $ GitHub.editCommentR owner repository commentId text
    pure
        $ foldl'
            (\acc (issueNumber, syncedComments)
             -> Map.adjust (addComments syncedComments) issueNumber acc)
            (Map.unions [unchangedIssues, newLocalIssues, editedLocalIssues])
        $ Map.toList
        $ Map.unionWith (<>) newLocalComments editedLocalComments
    where
        addComments :: Map (GitHub.Id Comment) LocalComment -> LocalIssue -> LocalIssue
        addComments syncedComments issue@LocalIssue {comments} =
            issue { comments = Map.union comments syncedComments }
