{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Recording where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Versioning

data Recording

instance FromField (Ref Recording)
