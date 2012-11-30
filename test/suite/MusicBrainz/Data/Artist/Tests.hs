{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (uk, acid2, male, person, portishead)

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor

import qualified MusicBrainz.Data.Relationship as Relationship

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testRelationships
        , testAliases
        , testIpiCodes
        , testAnnotation
        , testMerge
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  tree' <- tree <$> (entityRef <$> add uk)
                <*> (entityRef <$> add male)
                <*> (entityRef <$> add person)
  ClassTests.testCreateFindLatest tree'

  where
    tree country maleRef personRef =
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
                 , artistAliases = Set.empty
                 , artistIpiCodes = Set.empty
                 , artistRelationships = Set.empty
                 , artistAnnotation = ""
                 }


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change artist" $ mbTest $ do
  ClassTests.testUpdate freddie expected
  where
    expected = freddie { artistData = (artistData freddie)
                           { artistName = "LAX is boring"
                           , artistSortName = "I want to go home"
                           }
                       }


--------------------------------------------------------------------------------
testRelationships :: Test
testRelationships = testCase "Relationships are bidirectional over addition and deletion" $ mbTest $ do
  editor <- entityRef <$> register acid2
  rel <- expectedRel

  a <- create editor freddie
  b <- create editor (minimalTree portishead)

  edit1 <- createEdit $
    update editor (coreRevision a) freddie { artistRelationships = Set.singleton $ ArtistRelationship (coreRef b) rel }

  apply edit1

  relationshipsChanged a Set.empty (Set.singleton $ ArtistRelationship (coreRef b) rel)
  relationshipsChanged b Set.empty (Set.singleton $ ArtistRelationship (coreRef a) rel)

  changedA <- findLatest (coreRef a)
  changedB <- findLatest (coreRef b)

  edit2 <- createEdit $
    update editor (coreRevision $ changedB)
      (minimalTree portishead)

  apply edit2

  relationshipsChanged changedA (Set.singleton $ ArtistRelationship (coreRef b) rel) Set.empty
  relationshipsChanged changedB (Set.singleton $ ArtistRelationship (coreRef a) rel) Set.empty

  where
    expectedRel =
      Relationship <$> fmap entityRef (add $ RelationshipType "performer")
                   <*> pure Set.empty
                   <*> pure emptyDate
                   <*> pure emptyDate
                   <*> pure False

    relationshipsChanged for old new = do
      latest <- findLatest (coreRef for)
      oldRels <- Relationship.viewRelationships (coreRevision for)
      newRels <- Relationship.viewRelationships (coreRevision latest)

      liftIO $ do
        oldRels @?= old
        newRels @?= new


--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ mbTest $ do
  ClassTests.testAliases freddie alias
  where
    alias = Alias { aliasName = "Freddie"
                  , aliasSortName = "eidderF"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  }


--------------------------------------------------------------------------------
testIpiCodes :: Test
testIpiCodes = testCase "Can add and remove artist IPI codes" $ mbTest $ do
  editor <- entityRef <$> register acid2

  artist <- create editor freddie { artistIpiCodes = Set.singleton ipi }
  ipiPreUpdate <- viewIpiCodes (coreRevision artist)
  liftIO $ ipiPreUpdate @?= Set.singleton ipi

  edit <- createEdit $
    update editor (coreRevision artist) freddie

  apply edit

  latest <- findLatest (coreRef artist)
  ipiPostUpdate <- viewAliases (coreRevision latest)
  liftIO $ ipiPostUpdate @?= Set.empty

  where
    ipi = IPI "12345678912"


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation (return . const freddie)


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct artists" $ mbTest $ do
  editor <- entityRef <$> register acid2

  a <- create editor freddie
  b <- create editor (minimalTree portishead)

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveReference (dereference $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)


--------------------------------------------------------------------------------
freddie :: Tree Artist
freddie = ArtistTree
  { artistData =  Artist
      { artistName = "Freddie Mercury"
      , artistSortName = "Mercury, Freddie"
      , artistComment = "Of queen"
      , artistBeginDate =
          PartialDate (Just 1946) (Just 9) (Just 5)
      , artistEndDate =
          PartialDate (Just 1991) (Just 11) (Just 24)
      , artistEnded = True
      , artistGender = Nothing
      , artistCountry = Nothing
      , artistType = Nothing
      }
  , artistRelationships = Set.empty
  , artistAliases = Set.empty
  , artistIpiCodes = Set.empty
  , artistAnnotation = ""
  }

