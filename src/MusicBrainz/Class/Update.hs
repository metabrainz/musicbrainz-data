module MusicBrainz.Class.Update
    ( Update(update)
    ) where

import MusicBrainz.Class.NewEntityRevision
import MusicBrainz.Class.RealiseTree
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit
import MusicBrainz.Editor
import MusicBrainz.Ref
import MusicBrainz.Relationship.Internal
import MusicBrainz.Revision
import MusicBrainz.Tree

--------------------------------------------------------------------------------
{-| This type class allows one version of an entity to be replaced
(\'updated\') with another version. -}
class (Editable a, HoldsRelationships a, TreeRelationships a, RealiseTree a, NewEntityRevision a, ViewRevision a) => Update a where
  {-| Create a new version of an entity, linked to an existing 'Revision'
  (the parent) and use a new 'Tree'. -}
  update :: Ref Editor -> Ref (Revision a) -> Tree a -> EditT (Ref (Revision a))
  update editor baseRev artist = do
    revisionId <- runUpdate editor baseRev artist
    reflectRelationshipChanges editor baseRev artist
    return revisionId
