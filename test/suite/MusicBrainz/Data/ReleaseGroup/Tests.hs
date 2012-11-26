{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, acid2)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testFindLatest
        , testAnnotation
        ]


--------------------------------------------------------------------------------
testFindLatest :: Test
testFindLatest = testCase "findLatest when release group exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  created <- dummyTree editor >>= create editor
  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
dummyTree :: Ref Editor -> MusicBrainz (Tree ReleaseGroup)
dummyTree editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree (dummy ac)
