{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Edit.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

tests :: [Test]
tests = [ testAddEditNote ]

testAddEditNote :: Test
testAddEditNote = testCase "Add & retrieve edit notes for edits" $ mbTest $ do
  Just editor <- fmap entityRef <$> findEditorByName "acid2"
  editId <- openEdit
  addEditNote editId (expected editor)
  editNotes <- findEditNotes editId
  liftIO $ map entityData editNotes @?= [expected editor]
  where
    expected author = EditNote
        { editNoteBody = "I'm ocharles and I approve of this edit"
        , editNoteAuthor = author
        }
