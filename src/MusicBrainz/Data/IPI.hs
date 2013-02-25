{-| Functions for working with IPI codes. -}
module MusicBrainz.Data.IPI
    ( ViewIPICodes(..) ) where

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Implemented by types that can have IPI codes in their tree, and provides
methods to work with those IPI codes. -}
class ViewIPICodes a where
  {-| Fetch all IPI codes for a given revision of an entity. -}
  viewIpiCodes :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set IPI)

  findIpiCodes :: (Functor m, MonadIO m, Monad m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set IPI))
