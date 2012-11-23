{-| Functions for working with entity aliases. -}
module MusicBrainz.Data.Alias
    ( ViewAliases(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| This type class provides functions for working with aliases for specific
entity types. -}
class ViewAliases a where
  {-| Fetch all aliases for a given revision of an entity. -}
  viewAliases :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set Alias)
