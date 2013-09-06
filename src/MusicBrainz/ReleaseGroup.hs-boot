{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.ReleaseGroup where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data ReleaseGroup

instance FromField (Ref ReleaseGroup)
