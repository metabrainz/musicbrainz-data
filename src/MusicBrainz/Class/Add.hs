{-| Provides the 'Add' type class for adding new *unversioned* entities. -}
module MusicBrainz.Class.Add
    ( Add(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz.Monad
import MusicBrainz.Entity

--------------------------------------------------------------------------------
{-| The 'Add' type class allows you to add new entities that are not
versioned. -}
class Add a where
  {-| Add a new entity, with some starting data, producing a fresh 'Entity'
  with a 'Ref'. -}
  add :: (Functor m, MonadIO m) => a -> MusicBrainzT m (Entity a)
