{-| Provides the 'FindLatest' type class. -}
module MusicBrainz.Data.FindLatest
    ( FindLatest(..)
    , ResolveReference(..)
    ) where

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Monad
import MusicBrainz.Types

--------------------------------------------------------------------------------
{-| Attempt to find the latest revision of an entity (type @a@), by a given
'Ref'. To obtain the reference, you can use
'MusicBrainz.Data.Merge.resolveMbid'. -}
class FindLatest a where
  findLatest :: (Applicative m, Functor m, MonadIO m) =>
    Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))


--------------------------------------------------------------------------------
class ResolveReference a where
  {-| Attempt to resolve a reference from its attributes. If the attributes
  don't actually correspond to an entity in the database, then 'Nothing' is
  returned. -}
  resolveReference :: (Functor m, MonadIO m) => RefSpec a -> MusicBrainzT m (Maybe (Ref a))
