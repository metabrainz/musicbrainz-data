{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.URL where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data URL

instance FromField (Ref URL)
