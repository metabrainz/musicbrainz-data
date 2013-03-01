module MusicBrainz.Data.Relationship.Internal
    ( HoldsRelationships(..)
    , reflectRelationshipChanges
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.Set (Set)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Relationship (viewRelationships)
import MusicBrainz.Edit
import MusicBrainz.Lens

--------------------------------------------------------------------------------
{-| This type class is implemented for any entities that may have
'Relationship's. -}
class HoldsRelationships a where
  fetchEndPoints :: (Functor m, MonadIO m)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]

  reflectRelationshipChange ::
    Ref Editor -> Ref a -> (LinkedRelationship -> Set LinkedRelationship -> Set LinkedRelationship) -> LinkedRelationship -> EditM ()


--------------------------------------------------------------------------------
-- Reflect relationship changes against other entities
reflectRelationshipChanges :: (ViewRevision a, HoldsRelationships a, TreeRelationships a)
  => Ref Editor
  -> Ref (Revision a)
  -> Tree a
  -> EditM ()
reflectRelationshipChanges editor baseRev source = do
  oldRelationships <- viewRelationships baseRev
  let additions = (source^.relationships) `Set.difference` oldRelationships
  let deletions = oldRelationships `Set.difference` (source^.relationships)

  unless (Set.null additions && Set.null deletions) $ do
    self <- coreRef <$> viewRevision baseRev

    forM_ additions $
      reflectRelationshipChange editor self Set.insert

    forM_ deletions $
      reflectRelationshipChange editor self Set.delete
