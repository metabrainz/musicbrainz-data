{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Work where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Ref (Ref)

data Work

instance FromField (Ref Work)
