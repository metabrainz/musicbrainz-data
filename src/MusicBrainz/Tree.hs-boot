{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Tree (Tree, TreeRelationships(..)) where

import Control.Lens
import Data.Set (Set)
import Database.PostgreSQL.Simple.ToField (ToField)
import MusicBrainz.Ref (Ref)
import MusicBrainz.Relationship (LinkedRelationship)

data Tree a

instance ToField (Ref (Tree a))

class TreeRelationships a where
  relationships :: Lens' (Tree a) (Set LinkedRelationship)
