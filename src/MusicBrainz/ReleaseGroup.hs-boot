{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.ReleaseGroup where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Ref (Ref)

data ReleaseGroup

instance FromField (Ref ReleaseGroup)
