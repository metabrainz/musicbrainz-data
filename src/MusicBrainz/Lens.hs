{-| Various 'Lens'es to easily traverse MusicBrainz data types. -}
module MusicBrainz.Lens where

import Control.Lens

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Provide a single lens to view all relationships inside a 'Tree'. -}
class TreeRelationships a where
  {-| A 'Lens' into all relationships for any 'Tree'. -}
  relationships :: SimpleLens (Tree a) (Set.Set LinkedRelationship)


instance TreeRelationships Artist where
  relationships = lens getter setter
    where
      getter = artistRelationships
      setter tree new = tree { artistRelationships = new }


--------------------------------------------------------------------------------
{-| Provide a single lens to view all aliases inside a 'Tree'. -}
class TreeAliases a where
  {-| A 'Lens' into all relationships for any 'Tree'. -}
  aliases :: SimpleLens (Tree a) (Set.Set Alias)


instance TreeAliases Artist where
  aliases = lens getter setter
    where
      getter = artistAliases
      setter tree new = tree { artistAliases = new }

instance TreeAliases Label where
  aliases = lens getter setter
    where
      getter = labelAliases
      setter tree new = tree { labelAliases = new }
