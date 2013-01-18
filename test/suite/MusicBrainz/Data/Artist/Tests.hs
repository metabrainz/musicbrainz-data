{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import Data.Monoid (mempty)

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk, male, person, portishead, minimalTree, freddie)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testAliases
        , testIpiCodes
        , testAnnotation
        , testMerge
        , testResolveRevisionReference
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
                           PartialDate (Just 1946) (Just 9) (Just 5)
                       , artistEndDate =
                           PartialDate (Just 1991) (Just 11) (Just 24)
                       , artistEnded = True
                       , artistGender = Just maleRef
                       , artistCountry = Just country
                       , artistType = Just personRef
                       }
                   , artistAliases = mempty
                   , artistIpiCodes = mempty
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
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation (return . const freddie)


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct artists" $ do
  CommonTests.testMerge (pure . const (freddie, (minimalTree portishead)))

