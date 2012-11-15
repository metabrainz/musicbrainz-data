module MusicBrainz.Data.Create
    ( Create(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

class Create a where
  create :: (Functor m, MonadIO m)
    => Ref Editor -> Tree a -> MusicBrainzT m (CoreEntity a)
