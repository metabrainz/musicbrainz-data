{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Editor (findEditorByName)
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Label (create)

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when label exists" $ mbTest $ do
  Just editor <- findEditorByName "acid2"

  created <- create (entityRef editor) expected
  Just found <- findLatest (coreMbid created)

  liftIO $ found @?= created
  where
    expected = Label { labelName = "Revolution Records"
                     , labelSortName = "Records, Revolution"
                     , labelComment = ""
                     , labelBeginDate = emptyDate
                     , labelEndDate = emptyDate
                     , labelEnded = False
                     , labelType = Nothing
                     , labelCode = Nothing
                     }
