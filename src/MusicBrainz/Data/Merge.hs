module MusicBrainz.Data.Merge
    ( merge
    , Merge(..)
    ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision
import MusicBrainz.Edit

--------------------------------------------------------------------------------
{-| Merge an artist into another artist. -}
merge :: Merge a
  => Ref Editor -> Ref (Revision a) -> Ref a -> EditM (Ref (Revision a))
merge editor baseRev targetId = do
  -- Find the latest revision to merge into
  latestTarget <- findLatest targetId
  mergeInto <- cloneRevision latestTarget editor

  -- Link this revision to both the old tree and the latest version,
  -- and include it in the edit.
  includeRevision mergeInto
  addChild mergeInto baseRev
  addChild mergeInto (coreRevision latestTarget)

  return mergeInto


--------------------------------------------------------------------------------
class (Editable a, FindLatest a, CloneRevision a) => Merge a where
  {-| Attempt to resolve an 'MBID Artist' to a specific 'Artist' 'Ref'. This
  will follow merges to find the correct artist this MBID now points to. -}
  resolveMbid :: (Functor m, MonadIO m)
    => MBID a -> MusicBrainzT m (Maybe (Ref a))
