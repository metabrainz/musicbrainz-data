{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Label where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Ref (Ref)

data Label

instance FromField (Ref Label)
