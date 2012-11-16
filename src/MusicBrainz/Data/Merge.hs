module MusicBrainz.Data.Merge
    ( merge
    , Merge(..)
    ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

import MusicBrainz.Data.FindLatest
import MusicBrainz.Edit
import MusicBrainz.Data.Revision.Internal

--------------------------------------------------------------------------------
{-| Merge one entity into another. -}
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
{-| The merge type class allows 2 entities (with different references) to be
merged into one. -}
class (Editable a, FindLatest a, CloneRevision a) => Merge a where
  {-| Attempt to resolve an 'MBID' @a@ to a specific @a@ 'Ref'. This
  will follow merges to find the correct entity this MBID now points to, and
  thus the reference /may be different/ to the original MBID passed in. -}
  resolveMbid :: (Functor m, MonadIO m)
    => MBID a -> MusicBrainzT m (Maybe (Ref a))
