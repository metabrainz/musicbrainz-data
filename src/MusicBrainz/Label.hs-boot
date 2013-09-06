{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Label where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data Label

instance FromField (Ref Label)
