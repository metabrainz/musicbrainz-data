{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Url.Tests ( tests ) where

import Control.Applicative
import Data.Maybe (fromJust)
import Network.URI (parseURI)

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testMerge
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  ClassTests.testCreateFindLatest musicBrainz


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change url" $ mbTest $ do
  ClassTests.testUpdate musicBrainz google

--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct urls" $ mbTest $ do
  editor <- entityRef <$> register acid2

  a <- autoEdit $ create editor musicBrainz >>= viewRevision
  b <- autoEdit $ create editor google >>= viewRevision

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveReference (dereference $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)


--------------------------------------------------------------------------------
musicBrainz, google :: Tree Url
musicBrainz = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "https://musicbrainz.org/") } }
google = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "https://google.com/musicbrainz/rocks") } }
