module MusicBrainz.Data.IPI
    ( HasIPICodes(..) ) where

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
class HasIPICodes a where
  viewIpiCodes :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set IPI)

