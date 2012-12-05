{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Edit.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import MusicBrainz
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddEditNote
        , testVoteOnEdit
        , testResolveReference
        ]


--------------------------------------------------------------------------------
testAddEditNote :: Test
testAddEditNote = testCase "Add & retrieve edit notes for edits" $ mbTest $ do
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
testVoteOnEdit = testCase "Can submit votes on edits" $ mbTest $ do
  warp <- entityRef <$> register acid2 { editorName = "warp" }
  acid2 <- entityRef <$> register acid2

  editId <- openEdit

  voteOnEdit acid2 editId Accept
  voteOnEdit acid2 editId Reject
  voteOnEdit warp editId Reject

  [v1, v2, v3] <- listVotes editId
  liftIO $ print [v1, v2, v3]
  liftIO $ do
    assertVote v1 acid2 Accept True
    assertVote v2 acid2 Reject False
    assertVote v3 warp Reject False

  where
    assertVote vote editor score superceded = do
      voteEditor vote @?= editor
      voteVote vote @?= score
      voteSuperceded vote @?= superceded


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve edit references" $ mbTest $ do
  ClassTests.testResolveReference openEdit id
