{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Work.Tests ( tests ) where

import Control.Applicative
import Control.Lens

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2, minimalTree, wildRose)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor
import MusicBrainz.Data.Work

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        , testAliases
        , testAnnotation
        , testMerge
        , testIswc
        , testResolveRevisionReference
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
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference (return . const wildRose)
