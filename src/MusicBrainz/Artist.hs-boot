{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Artist where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data Artist

instance FromField (Ref Artist)
