{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Repository where

import Data.Maybe (fromJust)
import Data.Monoid
import Network.URI (parseURI)

import MusicBrainz
import MusicBrainz.Data

import Test.MusicBrainz
import Test.MusicBrainz.Data

portishead :: Artist
portishead = Artist { artistName = "Portishead"
                    , artistSortName = "Portishead"
                    , artistComment = ""
                    , artistBeginDate = emptyDate
                    , artistEndDate = emptyDate
                    , artistEnded = False
                    , artistGender = Nothing
                    , artistCountry = Nothing
                    , artistType = Nothing
                    }

dummy :: Ref ArtistCredit -> ReleaseGroup
dummy ac = ReleaseGroup { releaseGroupName = "Dummy"
                        , releaseGroupArtistCredit = ac
                        , releaseGroupComment = ""
                        , releaseGroupPrimaryType = Nothing
                        , releaseGroupSecondaryTypes = mempty
                        }

uk :: Country
uk = Country { countryName = "United Kingdom"
             , countryIsoCode = "gb"
             }

acid2 :: Editor
acid2 = Editor { editorName = "acid2", editorPassword = "fluffles" }

male :: Gender
male = Gender { genderName = "Male" }

person :: ArtistType
person = ArtistType { artistTypeName = "Person" }

latin :: Script
latin = Script { scriptName = "Latin", scriptIsoCode = "Latn", scriptIsoNumber = "215" }

english :: Language
english = Language
    { languageName = "English"
    , languageIsoCode2t = "eng"
    , languageIsoCode2b = "eng"
    , languageIsoCode1 = "en"
    , languageIsoCode3 = "eng"
    }

compilation :: ReleaseGroupType Secondary
compilation = ReleaseGroupType { releaseGroupTypeName = "Compilation" }

revolutionRecords :: Tree Label
revolutionRecords = minimalTree $
  Label { labelName = "Revolution Records"
        , labelSortName = "Records, Revolution"
        , labelComment = ""
        , labelBeginDate = emptyDate
        , labelEndDate = emptyDate
        , labelEnded = False
        , labelType = Nothing
        , labelCode = Nothing
        , labelCountry = Nothing
        }

mysterons :: Ref Editor -> MusicBrainz (Tree Recording)
mysterons editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree $
    Recording { recordingName = "Mysterons"
              , recordingComment = ""
              , recordingArtistCredit = ac
              , recordingDuration = Just 64936
              }

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
  , artistRelationships = mempty
  , artistAliases = mempty
  , artistIpiCodes = mempty
  , artistAnnotation = ""
  }


dummyReleaseTree :: Ref Editor -> MusicBrainz (Tree Release)
dummyReleaseTree editor = do
  portisheadAc <- singleArtistAc editor portishead
  portisheadRg <- autoEdit $ create editor (minimalTree (dummy portisheadAc)) >>= viewRevision
  return $ minimalTree $
    expected (coreRef portisheadRg) portisheadAc
  where
    expected rg ac =
      Release { releaseName = "Dummy"
              , releaseComment = ""
              , releaseArtistCredit = ac
              , releaseReleaseGroup = rg
              , releaseDate = PartialDate (Just 1997) (Just 9) (Just 29)
              , releaseCountry = Nothing
              , releaseScript = Nothing
              , releaseLanguage = Nothing
              , releasePackaging = Nothing
              , releaseStatus = Nothing
              }


dummyReleaseGroupTree :: Ref Editor -> MusicBrainz (Tree ReleaseGroup)
dummyReleaseGroupTree editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree (dummy ac)


wildRose :: Tree Work
wildRose = minimalTree Work { workName = "To a Wild Rose"
                            , workComment = ""
                            , workLanguage = Nothing
                            , workType = Nothing
                            }

musicBrainz, google :: Tree Url
musicBrainz = minimalTree Url { urlUrl = fromJust (parseURI "https://musicbrainz.org/") }
google = minimalTree Url { urlUrl = fromJust (parseURI "https://google.com/musicbrainz/rocks") }


--------------------------------------------------------------------------------
class MinimalTree a where
  minimalTree :: a -> Tree a

instance MinimalTree Artist where
  minimalTree dat' = ArtistTree dat' mempty mempty mempty ""

instance MinimalTree Label where
  minimalTree dat' = LabelTree dat' mempty mempty mempty ""

instance MinimalTree Recording where
  minimalTree dat' = RecordingTree dat' mempty "" mempty mempty

instance MinimalTree Release where
  minimalTree dat' = ReleaseTree dat' mempty "" mempty mempty

instance MinimalTree ReleaseGroup where
  minimalTree dat' = ReleaseGroupTree dat' mempty ""

instance MinimalTree Url where
  minimalTree dat' = UrlTree dat' mempty

instance MinimalTree Work where
  minimalTree dat' = WorkTree dat' mempty mempty "" mempty
