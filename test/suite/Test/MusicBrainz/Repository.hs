{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Repository where

import Data.Monoid

import MusicBrainz

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
acid2 = Editor { editorName = "acid2" }

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

--------------------------------------------------------------------------------
class MinimalTree a where
  minimalTree :: a -> Tree a

instance MinimalTree Artist where
  minimalTree dat' = ArtistTree dat' mempty mempty mempty ""

instance MinimalTree Label where
  minimalTree dat' = LabelTree dat' mempty mempty ""

instance MinimalTree Recording where
  minimalTree dat' = RecordingTree dat' "" mempty mempty

instance MinimalTree Release where
  minimalTree dat' = ReleaseTree dat' "" mempty mempty

instance MinimalTree ReleaseGroup where
  minimalTree dat' = ReleaseGroupTree dat' ""

instance MinimalTree Work where
  minimalTree dat' = WorkTree dat' mempty "" mempty
