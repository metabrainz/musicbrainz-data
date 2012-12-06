{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2, mysterons)

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
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  created <- mysterons editor >>= \t -> autoEdit $ create editor t >>= viewRevision
  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation mysterons


--------------------------------------------------------------------------------
testIsrc :: Test
testIsrc = testCase "Can add and remove ISRCs" $ mbTest $ do
  editor <- entityRef <$> register acid2

  tree <- mysterons editor

  recording <- autoEdit $ create editor (withIsrc tree) >>= viewRevision
  isrcPreUpdate <- viewIsrcs (coreRevision recording)
  liftIO $ isrcPreUpdate @?= Set.singleton isrc

  edit <- createEdit $
    update editor (coreRevision recording) tree

  apply edit

  latest <- findLatest (coreRef recording)
  isrcsPostUpdate <- viewIsrcs (coreRevision latest)
  liftIO $ isrcsPostUpdate @?= mempty

  where
    isrc = ISRC "GBAAA9800322"
    withIsrc t = t { recordingIsrcs = Set.singleton isrc }


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
