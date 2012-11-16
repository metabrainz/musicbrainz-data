{-| Provides the 'Create' type class for creating new entities. -}
module MusicBrainz.Data.Create
    ( Create(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

--------------------------------------------------------------------------------
{-| The create type class allows you to create new entities. -}
class Create a where
  {-| Create a new entity, with some starting data, producing a fresh MBID. -}
  create :: (Functor m, MonadIO m)
    => Ref Editor -> Tree a -> MusicBrainzT m (CoreEntity a)
