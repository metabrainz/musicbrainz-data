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
