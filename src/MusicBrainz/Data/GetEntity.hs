module MusicBrainz.Data.GetEntity (GetEntity(..)) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

class GetEntity a where
  getEntity :: (Functor m, MonadIO m) => Ref a -> MusicBrainzT m (Entity a)
