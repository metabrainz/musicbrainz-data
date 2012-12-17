{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc, minimalTree)
import Test.MusicBrainz.Repository (acid2, mysterons, portishead)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor (register)
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
testCreateFindLatest = testCase "findLatest when recording exists" $ mbTest $
  ClassTests.testCreateFindLatest mysterons


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove recording annotations" $ mbTest $ do
  ClassTests.testAnnotation mysterons


--------------------------------------------------------------------------------
testIsrc :: Test
testIsrc = testCase "Can add and remove ISRCs" $ mbTest $ do
  editor <- entityRef <$> register acid2

  tree <- mysterons editor

  recording <- autoEdit $ create editor (withIsrc tree) >>= viewRevision
  isrcPreUpdate <- viewIsrcs (coreRevision recording)
  liftIO $ isrcPreUpdate @?= Set.singleton expected

  edit <- createEdit $
    update editor (coreRevision recording) tree

  apply edit

  latest <- findLatest (coreRef recording)
  isrcsPostUpdate <- viewIsrcs (coreRevision latest)
  liftIO $ isrcsPostUpdate @?= mempty

  where
    expected = "GBAAA9800322" ^?! isrc
    withIsrc t = t { recordingIsrcs = Set.singleton expected }


--------------------------------------------------------------------------------
testPuid :: Test
testPuid = testCase "Can add and remove PUIDs" $ mbTest $ do
  editor <- entityRef <$> register acid2

  tree <- mysterons editor

  recording <- autoEdit $ create editor (withPuid tree) >>= viewRevision
  puidPreUpdate <- viewPuids (coreRevision recording)
  liftIO $ puidPreUpdate @?= Set.singleton expected

  edit <- createEdit $
    update editor (coreRevision recording) tree

  apply edit

  latest <- findLatest (coreRef recording)
  puidsPostUpdate <- viewPuids (coreRevision latest)
  liftIO $ puidsPostUpdate @?= mempty

  where
    expected = fromJust ("3893a0d0-3fd1-11e2-a25f-0800200c9a66" ^? puid)
    withPuid t = t { recordingPuids = Set.singleton expected }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct recordings" $ mbTest $ do
  ClassTests.testMerge createRecordings
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
testResolveRevisionReference = testCase "Resolve revision reference" $ mbTest $ do
  ClassTests.testResolveRevisionReference mysterons
