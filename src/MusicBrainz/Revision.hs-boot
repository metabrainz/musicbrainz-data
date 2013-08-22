{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Revision where

import Database.PostgreSQL.Simple.FromField

import MusicBrainz.Ref

data Revision a 

instance FromField (Ref (Revision a))
