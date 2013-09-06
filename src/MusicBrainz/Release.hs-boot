{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Release where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import MusicBrainz.Versioning

data Release
instance Eq Release
instance Show Release

data Track
instance Eq Track
instance Show Track

instance FromField (Ref Release)

instance FromRow Release

instance FromRow Track
