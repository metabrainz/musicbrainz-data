{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Repository where

import MusicBrainz

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
