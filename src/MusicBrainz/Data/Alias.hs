module MusicBrainz.Data.Alias
    ( HasAliases(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

import qualified Data.Set as Set

class HasAliases a where
  viewAliases :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set Alias)
