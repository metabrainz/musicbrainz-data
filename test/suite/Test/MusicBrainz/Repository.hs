{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Repository where

import Control.Lens
import Data.Maybe (fromJust)
import Data.Monoid
import Network.URI (parseURI)

import MusicBrainz.Monad
import MusicBrainz.Artist
import MusicBrainz.ArtistCredit
import MusicBrainz.Country
import MusicBrainz.Gender
import MusicBrainz.Label
import MusicBrainz.Language
import MusicBrainz.PartialDate
import MusicBrainz.Recording
import MusicBrainz.Relationship
import MusicBrainz.Release
import MusicBrainz.ReleaseGroup
import MusicBrainz.Script
import MusicBrainz.URL
import MusicBrainz.Versioning
import MusicBrainz.Work

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

mysterons :: Ref Editor -> MusicBrainz (Tree Recording)
mysterons editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree $
    Recording { recordingName = "Mysterons"
              , recordingComment = ""
              , recordingArtistCredit = ac
              , recordingDuration = Just 64936
              }

wildRose :: Tree Work
wildRose = minimalTree Work { workName = "To a Wild Rose"
                            , workComment = ""
                            , workLanguage = Nothing
                            , workType = Nothing
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

freddie :: Tree Artist
freddie = ArtistTree
  { artistData =  Artist
      { artistName = "Freddie Mercury"
      , artistSortName = "Mercury, Freddie"
      , artistComment = "Of queen"
      , artistBeginDate = (Just 1946, Just 9, Just 5) ^?! partialDate
      , artistEndDate = (Just 1991, Just 11, Just 24) ^?! partialDate
      , artistEnded = True
      , artistGender = Nothing
      , artistCountry = Nothing
      , artistType = Nothing
      }
  , artistRelationships = mempty
  , artistAliases = mempty
  , artistIpiCodes = mempty
  , artistIsniCodes= mempty
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
              , releaseDate = (Just 1997, Just 9, Just 29) ^?! partialDate
              , releaseCountry = Nothing
              , releaseScript = Nothing
              , releaseLanguage = Nothing
              , releasePackaging = Nothing
              , releaseStatus = Nothing
              , releaseBarcode = Just NoBarcode
              }


dummyReleaseGroupTree :: Ref Editor -> MusicBrainz (Tree ReleaseGroup)
dummyReleaseGroupTree editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree (dummy ac)

musicBrainz, google :: Tree URL
musicBrainz = minimalTree URL { urlUrl = fromJust (parseURI "https://musicbrainz.org/") }
google = minimalTree URL { urlUrl = fromJust (parseURI "https://google.com/musicbrainz/rocks") }

--------------------------------------------------------------------------------
class MinimalTree a where
  minimalTree :: a -> Tree a

instance MinimalTree Artist where
  minimalTree dat' = ArtistTree dat' mempty mempty mempty mempty ""

instance MinimalTree Label where
  minimalTree dat' = LabelTree dat' mempty mempty mempty mempty ""

instance MinimalTree Recording where
  minimalTree dat' = RecordingTree dat' mempty "" mempty

instance MinimalTree Release where
  minimalTree dat' = ReleaseTree dat' mempty "" mempty mempty

instance MinimalTree ReleaseGroup where
  minimalTree dat' = ReleaseGroupTree dat' mempty ""

instance MinimalTree URL where
  minimalTree dat' = URLTree dat' mempty

instance MinimalTree Work where
  minimalTree dat' = WorkTree dat' mempty mempty "" mempty

performer :: RelationshipType
performer = RelationshipType { relName = "performer"
                             , relTypeAttributes = mempty
                             , relParent = Nothing
                             , relLeftTarget = ToArtist
                             , relRightTarget = ToRecording
                             , relLinkPhrase = "performed"
                             , relReverseLinkPhrase = "was performed by"
                             , relShortLinkPhrase = "performed"
                             , relPriority = 0
                             , relChildOrder = 0
                             , relDescription = "Indicates an artist performed a recording"
                             }

additional :: MusicBrainz (Entity RelationshipAttribute)
additional = addRelationshipAttributeType "performer" Nothing Nothing 0 ""
