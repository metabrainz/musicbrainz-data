module MusicBrainz.Data.Update
    ( Update(update)
    ) where

import MusicBrainz
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Edit
import MusicBrainz.Lens

--------------------------------------------------------------------------------
{-| This type class allows one version of an entity to be replaced
(\'updated\') with another version. -}
class (Editable a, HoldsRelationships a, TreeRelationships a) => Update a where
  {-| Create a new version of an entity, linked to an existing 'Revision'
  (the parent) and use a new 'Tree'. -}
  update :: Ref Editor -> Ref (Revision a) -> Tree a -> EditM (Ref (Revision a))
  update editor baseRev artist = do
    revisionId <- runUpdate editor baseRev artist
    reflectRelationshipChanges editor baseRev artist
    return revisionId

