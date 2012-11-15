module MusicBrainz.Data.Merge
    ( merge ) where

import MusicBrainz

import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision
import MusicBrainz.Edit


--------------------------------------------------------------------------------
{-| Merge an artist into another artist. -}
merge :: (Editable a, FindLatest a, CloneRevision a)
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
