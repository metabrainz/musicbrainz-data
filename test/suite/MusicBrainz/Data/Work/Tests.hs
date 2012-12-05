{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Work.Tests ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor
import MusicBrainz.Data.Work

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testAliases
        , testAnnotation
        , testMerge
        , testIswc
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  ClassTests.testCreateFindLatest tree
  where
    tree = minimalTree Work
             { workName = "Freddie Mercury"
             , workComment = "Of queen"
             , workType = Nothing
             , workLanguage = Nothing
             }


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change work" $ mbTest $ do
  ClassTests.testUpdate wildRose expected
   where
    expected = wildRose { workData = (workData wildRose)
                            { workName = "TO A WILD ROSE"
                            }
                        }



--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ mbTest $ do
  ClassTests.testAliases wildRose alias
  where
    alias = Alias { aliasName = "T⊙ Å w¥ł≙ ℜøßė"
                  , aliasSortName = "toawildrose"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  }


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove work annotations" $ mbTest $ do
  ClassTests.testAnnotation (return . const wildRose)


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct works" $ mbTest $ do
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
testIswc = testCase "Can add and remove ISWCs" $ mbTest $ do
  editor <- entityRef <$> register acid2

  work <- autoEdit $ create editor wildRoseWithIswc >>= viewRevision
  iswcPreUpdate <- viewIswcs (coreRevision work)
  liftIO $ iswcPreUpdate @?= Set.singleton iswc

  edit <- createEdit $
    update editor (coreRevision work) wildRose

  apply edit

  latest <- findLatest (coreRef work)
  iswcsPostUpdate <- viewIswcs (coreRevision latest)
  liftIO $ iswcsPostUpdate @?= Set.empty

  where
    iswc = ISWC "T-070.116.442-2"
    wildRoseWithIswc = wildRose { workIswcs = Set.singleton iswc }


--------------------------------------------------------------------------------
wildRose :: Tree Work
wildRose = minimalTree Work { workName = "To a Wild Rose"
                            , workComment = ""
                            , workLanguage = Nothing
                            , workType = Nothing
                            }
