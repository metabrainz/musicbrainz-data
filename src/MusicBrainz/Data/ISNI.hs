{-| Functions for working with ISNI codes. -}
module MusicBrainz.Data.ISNI
    ( ViewISNICodes(..) ) where

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Implemented by types that can have ISNI codes in their tree, and provides
methods to work with those ISNI codes. -}
class ViewISNICodes a where
  {-| Fetch all ISNI codes for a set of revisions. -}
  viewIsniCodes :: (Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set ISNI))
