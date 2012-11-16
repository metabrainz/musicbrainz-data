{-| Functions for working with IPI codes. -}
module MusicBrainz.Data.IPI
    ( HasIPICodes(..) ) where

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Implemented by types that can have IPI codes in their tree, and provides
methods to work with those IPI codes. -}
class HasIPICodes a where
  {-| Fetch all IPI codes for a given revision of an entity. -}
  viewIpiCodes :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set IPI)

