{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Url.Tests ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Create
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit
import MusicBrainz.EditApplication
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.Ref
import MusicBrainz.Class.ResolveReference

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testMerge
        , testResolveRevisionReference
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ do
  CommonTests.testCreateFindLatest (return . const musicBrainz)


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change url" $ do
  CommonTests.testUpdate musicBrainz google

--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct urls" $ do
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
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference (return . const musicBrainz)
