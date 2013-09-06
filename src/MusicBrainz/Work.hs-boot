{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Work where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data Work

instance FromField (Ref Work)
