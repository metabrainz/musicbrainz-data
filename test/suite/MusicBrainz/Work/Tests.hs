{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Work.Tests ( tests ) where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Data.Monoid (mempty)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Alias
import MusicBrainz.ArtistCredit
import MusicBrainz.EditApplication
import MusicBrainz.ISWC
import MusicBrainz.PartialDate
import MusicBrainz.Recording (recordingArtistCredit)
import MusicBrainz.Relationship
import MusicBrainz.Util (viewOnce)
import MusicBrainz.Work
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testAliases
        , testAnnotation
        , testMerge
        , testIswc
        , testResolveRevisionReference
        , testFindIswcs
        , testFindByArtist
        , testFindByIswc
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ do
  CommonTests.testCreateFindLatest (return . const tree)
  where
    tree = minimalTree Work
             { workName = "Freddie Mercury"
             , workComment = "Of queen"
             , workType = Nothing
             , workLanguage = Nothing
             }


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change work" $ do
  CommonTests.testUpdate wildRose expected
   where
    expected = wildRose { workData = (workData wildRose)
                            { workName = "TO A WILD ROSE"
                            }
                        }



--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ do
  CommonTests.testAliases wildRose alias
  where
    alias = Alias { aliasName = "T⊙ Å w¥ł≙ ℜøßė"
                  , aliasSortName = "toawildrose"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  , aliasPrimaryForLocale = False
                  }


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove work annotations" $ do
  CommonTests.testAnnotation (return . const wildRose)


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct works" $ do
  editor <- entityRef <$> register acid2

  a <- autoEdit $ create editor wildRose >>= viewRevision
  b <- autoEdit $ create editor wildRose >>= viewRevision

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveReference (dereference $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)


--------------------------------------------------------------------------------
testIswc :: Test
testIswc = testCase "Can add and remove ISWCs" $
  CommonTests.createAndUpdateSubtree
    (return . const wildRose)
    withIswc
    workIswcs
    viewIswcs

  where
    expected = "T-070.116.442-2" ^?! iswc
    withIswc x = x { workIswcs = Set.singleton expected }


--------------------------------------------------------------------------------
testFindByIswc :: Test
testFindByIswc = testCase "Can find works by ISWCs" $ do
    editor <- entityRef <$> register acid2
    createEdit $ do
      work <- viewRevision =<< create editor (withIswc honeysuckleRose)
      actual <- findByIswc expected
      actual @?= [ work ]
  where
    expected = "T-070.116.442-2" ^?! iswc
    withIswc x = x { workIswcs = Set.singleton expected }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference (return . const wildRose)


--------------------------------------------------------------------------------
testFindIswcs :: Test
testFindIswcs = testCase "Can find ISWCs for multiple works" $ do
  editor <- entityRef <$> register acid2
  void $ createEdit $ do
    revId <- create editor honeysuckleRose { workIswcs = expected }
    iswcs <- findIswcs $ Set.singleton revId
    liftIO $ iswcs @?= Map.singleton revId expected

  where
    expected = Set.singleton $ "T-070.074.170-3"^?!iswc


honeysuckleRose :: Tree Work
honeysuckleRose = minimalTree
  Work { workName = "Honeysuckle Rose"
       , workComment = ""
       , workType = Nothing
       , workLanguage = Nothing
       }

--------------------------------------------------------------------------------
testFindByArtist :: Test
testFindByArtist = testGroup "Find works by artists"
  [ testCase "Find by recording artist" $ do
      editor <- entityRef <$> register acid2
      recTree <- mysterons editor
      createEdit $ do
        recording <- create editor recTree >>= viewRevision
        artist <- acnArtist . head <$> viewOnce expandCredits
          (recordingArtistCredit . coreData $ recording)

        performance <- add RelationshipType
          { relName = "performance"
          , relTypeAttributes = mempty
          , relParent = Nothing
          , relLeftTarget = ToRecording
          , relRightTarget = ToWork
          , relLinkPhrase = "performance"
          , relReverseLinkPhrase = "has performance"
          , relShortLinkPhrase = "performance"
          , relPriority = 0
          , relChildOrder = 0
          , relDescription = ""
          }

        work <- viewRevision =<< create editor honeysuckleRose
          { workRelationships = Set.singleton $
              RecordingRelationship (coreRef recording)
                Relationship { relType = entityRef performance
                             , relAttributes = mempty
                             , relBeginDate = emptyDate
                             , relEndDate = emptyDate
                             , relEnded = False
                             }
          }

        actual <- findByArtist artist
        actual @?= [ work ]

  , testCase "Find by artist relationship" $ do
      editor <- entityRef <$> register acid2
      createEdit $ do
        artist <- fmap coreRef . viewRevision =<< create editor freddie

        composer <- add RelationshipType
          { relName = "composer"
          , relTypeAttributes = mempty
          , relParent = Nothing
          , relLeftTarget = ToArtist
          , relRightTarget = ToWork
          , relLinkPhrase = "composed"
          , relReverseLinkPhrase = "was composed by"
          , relShortLinkPhrase = "composer"
          , relPriority = 0
          , relChildOrder = 0
          , relDescription = ""
          }

        work <- viewRevision =<< create editor honeysuckleRose
          { workRelationships = Set.singleton $
              ArtistRelationship artist
                Relationship { relType = entityRef composer
                             , relAttributes = mempty
                             , relBeginDate = emptyDate
                             , relEndDate = emptyDate
                             , relEnded = False
                             }
          }

        actual <- findByArtist artist
        actual @?= [ work ]
  ]
