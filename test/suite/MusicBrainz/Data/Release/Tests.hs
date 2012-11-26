{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Release.Tests
    ( tests ) where

import Control.Applicative
import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, uk, acid2)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor (register)

import qualified MusicBrainz.Data.Country as Country
import qualified MusicBrainz.Data.Language as Language
import qualified MusicBrainz.Data.ReleasePackaging as ReleasePackaging
import qualified MusicBrainz.Data.ReleaseStatus as ReleaseStatus
import qualified MusicBrainz.Data.Script as Script

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when release exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  created <- dummyTree editor >>= create editor

  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
dummyTree :: Ref Editor -> MusicBrainz (Tree Release)
dummyTree editor = do
  portisheadAc <- singleArtistAc editor portishead
  portisheadRg <- create editor (minimalTree (dummy portisheadAc))
  country <- Country.addCountry uk
  script <- Script.addScript Script { scriptName = "United Kingdom"
                                    , scriptIsoCode = "gb"
                                    , scriptIsoNumber = "360"
                                    }
  language <- Language.addLanguage Language
    { languageName = "United Kingdom"
    , languageIsoCode2t = "gb"
    , languageIsoCode2b = ""
    , languageIsoCode1 = ""
    , languageIsoCode3 = ""
    }
  packaging <- ReleasePackaging.addReleasePackaging ReleasePackaging
    { releasePackagingName = "Jewel Case" }
  status <- ReleaseStatus.addReleaseStatus ReleaseStatus
    { releaseStatusName = "Official" }
  return $ minimalTree $
    expected (coreRef portisheadRg) portisheadAc
      (entityRef country) (entityRef script) (entityRef language)
      (entityRef packaging) (entityRef status)
  where
    expected rg ac country script language packaging status =
      Release { releaseName = "Dummy"
              , releaseComment = ""
              , releaseArtistCredit = ac
              , releaseReleaseGroup = rg
              , releaseDate = PartialDate (Just 1997) (Just 9) (Just 29)
              , releaseCountry = Just country
              , releaseScript = Just script
              , releaseLanguage = Just language
              , releasePackaging = Just packaging
              , releaseStatus = Just status
              }
