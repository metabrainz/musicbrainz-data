{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Work.Tests ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

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
        , testAliases
        , testAnnotation
        , testMerge
        ]

--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  ClassTests.testCreateFindLatest tree
  where
    tree =
      WorkTree { workData = Work
                     { workName = "Freddie Mercury"
                     , workComment = "Of queen"
                     , workType = Nothing
                     , workLanguage = Nothing
                     }
                 , workAliases = Set.empty
                 , workAnnotation = ""
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
wildRose :: Tree Work
wildRose = WorkTree { workData = Work { workName = "To a Wild Rose"
                                      , workComment = ""
                                      , workLanguage = Nothing
                                      , workType = Nothing
                                      }
                    , workAliases = Set.empty
                    , workAnnotation = ""
                    }
