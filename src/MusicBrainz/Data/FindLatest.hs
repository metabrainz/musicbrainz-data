{-| Provides the 'FindLatest' type class. -}
module MusicBrainz.Data.FindLatest
    ( FindLatest(..)
    ) where

import Control.Monad.IO.Class

import MusicBrainz

{-| Attempt to find the latest revision of an entity (type @a@), by a given
MBID. If there is no entity with this MBID, then 'Nothing' is returned. This
function will also follow redirection chains, so the returned entity may have a
/different/ MBID than the input. -}
class FindLatest a where
  findLatest :: (Functor m, MonadIO m) => Ref a -> MusicBrainzT m (CoreEntity a)
