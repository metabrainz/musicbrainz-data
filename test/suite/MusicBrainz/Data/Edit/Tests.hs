{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Edit.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddEditNote
        , testVoteOnEdit
        , testResolveReference
        ]


--------------------------------------------------------------------------------
testAddEditNote :: Test
testAddEditNote = testCase "Add & retrieve edit notes for edits" $ do
  editor <- entityRef <$> register acid2
  editId <- openEdit
  addEditNote editId (expected editor)
  editNotes <- findEditNotes editId
  liftIO $ map entityData editNotes @?= [expected editor]
  where
    expected author = EditNote
        { editNoteBody = "I'm ocharles and I approve of this edit"
        , editNoteAuthor = author
        }


--------------------------------------------------------------------------------
testVoteOnEdit :: Test
testVoteOnEdit = testCase "Can submit votes on edits" $ do
  warpId <- entityRef <$> register acid2 { editorName = "warp" }
  acid2Id <- entityRef <$> register acid2

  editId <- openEdit

  voteOnEdit acid2Id editId Accept
  voteOnEdit acid2Id editId Reject
  voteOnEdit warpId editId Reject

  [v1, v2, v3] <- listVotes editId
  liftIO $ print [v1, v2, v3]
  liftIO $ do
    assertVote v1 acid2Id Accept True
    assertVote v2 acid2Id Reject False
    assertVote v3 warpId Reject False

  where
    assertVote vote editor score superceded = do
      voteEditor vote @?= editor
      voteVote vote @?= score
      voteSuperceded vote @?= superceded


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve edit references" $ do
  CommonTests.testResolveReference openEdit id
