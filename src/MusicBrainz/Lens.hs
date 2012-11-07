module MusicBrainz.Lens where

import Control.Lens

import qualified Data.Set as Set

import MusicBrainz

class TreeRelationships a where
  relationships :: SimpleLens (Tree a) (Set.Set Relationship)

instance TreeRelationships Artist where
  relationships = lens artistRelationships (\tree new -> tree { artistRelationships = new })
