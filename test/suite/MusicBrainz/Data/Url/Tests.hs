{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Url.Tests ( tests ) where

import Control.Applicative
import Data.Maybe (fromJust)
import Network.URI (parseURI)

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testMerge
        , testResolveRevisionReference
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  CommonTests.testCreateFindLatest (return . const musicBrainz)


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change url" $ mbTest $ do
  CommonTests.testUpdate musicBrainz google

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
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ mbTest $ do
  CommonTests.testResolveRevisionReference (return . const musicBrainz)


--------------------------------------------------------------------------------
musicBrainz, google :: Tree Url
musicBrainz = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "https://musicbrainz.org/") } }
google = UrlTree { urlData = Url { urlUrl = fromJust (parseURI "https://google.com/musicbrainz/rocks") } }
