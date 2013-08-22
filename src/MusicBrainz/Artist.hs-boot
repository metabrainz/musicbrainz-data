{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Artist where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Ref (Ref)

data Artist

instance FromField (Ref Artist)
