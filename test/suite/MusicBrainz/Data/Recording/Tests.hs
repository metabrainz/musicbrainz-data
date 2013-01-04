{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Lens
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc)
import Test.MusicBrainz.Repository (mysterons, portishead, minimalTree)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data.Recording

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testIsrc
        , testPuid
        , testMerge
        , testResolveRevisionReference
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $
  CommonTests.testCreateFindLatest mysterons


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove recording annotations" $ do
  CommonTests.testAnnotation mysterons


--------------------------------------------------------------------------------
testIsrc :: Test
testIsrc = testCase "Can add and remove ISRCs" $
  CommonTests.createAndUpdateSubtree
    mysterons
    withIsrc
    recordingIsrcs
    viewIsrcs

  where
    expected = "GBAAA9800322" ^?! isrc
    withIsrc t = t { recordingIsrcs = Set.singleton expected }


--------------------------------------------------------------------------------
testPuid :: Test
testPuid = testCase "Can add and remove PUIDs" $
  CommonTests.createAndUpdateSubtree
    mysterons
    withPuid
    recordingPuids
    viewPuids

  where
    expected = fromJust ("3893a0d0-3fd1-11e2-a25f-0800200c9a66" ^? puid)
    withPuid t = t { recordingPuids = Set.singleton expected }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct recordings" $ do
  CommonTests.testMerge createRecordings
  where
    createRecordings editor = do
      a <- mysterons editor
      b <- strangers editor
      return (a, b)


--------------------------------------------------------------------------------
strangers :: Ref Editor -> MusicBrainz (Tree Recording)
strangers editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree $
    Recording { recordingName = "Strangers"
              , recordingComment = ""
              , recordingArtistCredit = ac
              , recordingDuration = Nothing
              }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference mysterons
