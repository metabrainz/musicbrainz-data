{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Artist.Tests ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)


import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Alias
import MusicBrainz.Artist
import MusicBrainz.Entity
import MusicBrainz.PartialDate

{-
import qualified Data.Set as Set
import MusicBrainz.Class.Create
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Relationship
import MusicBrainz.Revision
import MusicBrainz.Tree
-}

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testAliases
        , testIpiCodes
        , testIsniCodes
        , testAnnotation
        , testMerge
        , testResolveRevisionReference
        --, testEligibleForCleanup
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ do
  CommonTests.testCreateFindLatest (const tree)

  where
    tree = do
      country <- entityRef <$> add uk
      maleRef <- entityRef <$> add male
      personRef <- entityRef <$> add person
      return $
        ArtistTree { artistData = Artist
                       { artistName = "Freddie Mercury"
                       , artistSortName = "Mercury, Freddie"
                       , artistComment = "Of queen"
                       , artistBeginDate =
                           (Just 1946, Just 9, Just 5) ^?! partialDate
                       , artistEndDate =
                           (Just 1991, Just 11, Just 24) ^?! partialDate
                       , artistEnded = True
                       , artistGender = Just maleRef
                       , artistCountry = Just country
                       , artistType = Just personRef
                       }
                   , artistAliases = mempty
                   , artistIpiCodes = mempty
                   , artistIsniCodes = mempty
                   , artistRelationships = mempty
                   , artistAnnotation = ""
                   }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference (return . const freddie)


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change artist" $ do
  CommonTests.testUpdate freddie expected
  where
    expected = freddie { artistData = (artistData freddie)
                           { artistName = "LAX is boring"
                           , artistSortName = "I want to go home"
                           }
                       }


--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ do
  CommonTests.testAliases freddie alias
  where
    alias = Alias { aliasName = "Freddie"
                  , aliasSortName = "eidderF"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  , aliasPrimaryForLocale = False
                  }


--------------------------------------------------------------------------------
testIpiCodes :: Test
testIpiCodes = testCase "Can add and remove artist IPI codes" $ do
  CommonTests.testIpiCodes freddie


--------------------------------------------------------------------------------
testIsniCodes :: Test
testIsniCodes = testCase "Can add and remove artist ISNI codes" $ do
  CommonTests.testIsniCodes freddie


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation (return . const freddie)


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct artists" $ do
  CommonTests.testMerge (pure . const (freddie, (minimalTree portishead)))


{-
--------------------------------------------------------------------------------
testEligibleForCleanup :: Test
testEligibleForCleanup = testCase "Artist with relationships is not eligible for cleanup" $ do
    CommonTests.testEligibleForCleanup makeTree
  where
    makeTree editor = do
      a <- autoEdit $ create editor (minimalTree portishead) >>= viewRevision
      relationshipMeta <-
        Relationship <$> fmap entityRef (add performer)
                     <*> pure mempty
                     <*> pure emptyDate
                     <*> pure emptyDate
                     <*> pure False
      let rel = ArtistRelationship (coreRef a) relationshipMeta
      return $ relationships .~ Set.singleton rel $ freddie
-}
