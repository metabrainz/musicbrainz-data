{-| Provides the 'FindLatest' type class. -}
module MusicBrainz.Data.FindLatest
    ( FindLatest(..)
    ) where

import Control.Monad.IO.Class

import MusicBrainz

--------------------------------------------------------------------------------
{-| Attempt to find the latest revision of an entity (type @a@), by a given
'Ref'. To obtain the reference, you can use
'MusicBrainz.Data.Merge.resolveMbid'. -}
class FindLatest a where
  findLatest :: (Functor m, MonadIO m) => Ref a -> MusicBrainzT m (CoreEntity a)
