{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, acid2)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor (register)

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
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
mysterons :: Ref Editor -> MusicBrainz (Tree Recording)
mysterons editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree $
    Recording { recordingName = "Mysterons"
              , recordingComment = ""
              , recordingArtistCredit = ac
              , recordingDuration = 64936
              }
