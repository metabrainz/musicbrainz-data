{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk, acid2, male, person, portishead)

import MusicBrainz
import MusicBrainz.Data.Alias
import MusicBrainz.Data.Artist
import MusicBrainz.Data.Create
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor (register)
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Revision
import MusicBrainz.Data.Update

import qualified MusicBrainz.Data.ArtistType as ArtistType
import qualified MusicBrainz.Data.Country as Country
import qualified MusicBrainz.Data.Gender as Gender
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
  tree' <- tree <$> (entityRef <$> Country.addCountry uk)
                <*> (entityRef <$> Gender.addGender male)
                <*> (entityRef <$> ArtistType.addArtistType person)
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
  editor <- entityRef <$> register acid2

  created <- create editor freddie
  let artistId = coreRef created

  editId <- createEdit $
    update editor (coreRevision created) expected

  apply editId

  found <- findLatest artistId
  latestTree <- viewTree (coreRevision found)
  liftIO $ latestTree @?= expected

  parents <- revisionParents (coreRevision found)
  liftIO $
    coreRevision created `Set.member` parents @? "Is parented to starting revision"

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
  b <- create editor (ArtistTree portishead Set.empty Set.empty Set.empty "")

  edit1 <- createEdit $
    update editor (coreRevision a) freddie { artistRelationships = Set.singleton $ ArtistRelationship (coreRef b) rel }

  apply edit1

  relationshipsChanged a Set.empty (Set.singleton $ ArtistRelationship (coreRef b) rel)
  relationshipsChanged b Set.empty (Set.singleton $ ArtistRelationship (coreRef a) rel)

  changedA <- findLatest (coreRef a)
  changedB <- findLatest (coreRef b)

  edit2 <- createEdit $
    update editor (coreRevision $ changedB)
      (ArtistTree portishead Set.empty Set.empty Set.empty "")

  apply edit2

  relationshipsChanged changedA (Set.singleton $ ArtistRelationship (coreRef b) rel) Set.empty
  relationshipsChanged changedB (Set.singleton $ ArtistRelationship (coreRef a) rel) Set.empty

  where
    expectedRel =
      Relationship <$> fmap entityRef (Relationship.addRelationshipType $ RelationshipType "performer")
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
  editor <- entityRef <$> register acid2

  artist <- create editor freddie { artistAliases = Set.singleton alias }
  aliasesPreUpdate <- viewAliases (coreRevision artist)
  liftIO $ aliasesPreUpdate @?= Set.singleton alias

  edit <- createEdit $
    update editor (coreRevision artist) freddie

  apply edit

  latest <- findLatest (coreRef artist)
  aliasesPostUpdate <- viewAliases (coreRevision latest)
  liftIO $ aliasesPostUpdate @?= Set.empty

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
  editor <- entityRef <$> register acid2

  artist <- create editor freddie { artistAnnotation = expected }
  annPreUpdate <- viewAnnotation (coreRevision artist)
  liftIO $ annPreUpdate @?= expected

  edit <- createEdit $
    update editor (coreRevision artist) freddie

  apply edit

  latest <- findLatest (coreRef artist)
  annPostUpdate <- viewAnnotation (coreRevision latest)
  liftIO $ annPostUpdate @?= ""

  where
    expected = "This is an artist annotation"


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct artists" $ mbTest $ do
  editor <- entityRef <$> register acid2

  a <- create editor freddie
  b <- create editor (ArtistTree portishead Set.empty Set.empty Set.empty "")

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveMbid (refMbid $ coreRef a)
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

