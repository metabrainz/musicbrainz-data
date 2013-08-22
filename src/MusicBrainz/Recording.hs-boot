{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Recording where

import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Ref (Ref)

data Recording

instance FromField (Ref Recording)
