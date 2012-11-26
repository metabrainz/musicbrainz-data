module MusicBrainz.Data.Merge
    ( merge
    , Merge
    ) where

import MusicBrainz

import MusicBrainz.Data.FindLatest
import MusicBrainz.Edit
import MusicBrainz.Data.Revision.Internal

class (CloneRevision a, Editable a, NewEntityRevision a) => Merge a

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
