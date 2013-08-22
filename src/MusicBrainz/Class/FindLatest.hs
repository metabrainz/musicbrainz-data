module MusicBrainz.Class.FindLatest ( FindLatest(..) ) where

import Control.Applicative
import Control.Monad.IO.Class

import MusicBrainz.Monad
import MusicBrainz.Entity
import MusicBrainz.Ref (Ref)

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| Attempt to find the latest revision of an entity (type @a@), by a given
'Ref'. To obtain the reference, you can use
'MusicBrainz.Merge.resolveMbid'. -}
class FindLatest a where
  findLatest :: (Applicative m, Functor m, MonadIO m) =>
    Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))
