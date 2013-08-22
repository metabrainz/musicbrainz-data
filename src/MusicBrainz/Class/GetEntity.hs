module MusicBrainz.Class.GetEntity where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz.Monad
import MusicBrainz.Entity (Entity)
import MusicBrainz.Ref (Ref)

--------------------------------------------------------------------------------
class GetEntity a where
  getEntity :: (Functor m, MonadIO m) => Ref a -> MusicBrainzT m (Entity a)
