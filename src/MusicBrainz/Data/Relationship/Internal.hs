{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Data.Relationship.Internal
    ( HoldsRelationships(..)
    , reflectRelationshipChanges
    ) where

import Control.Applicative
import Control.Lens hiding (cons)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Data.Foldable (forM_)
import Data.Set (Set)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.CoreEntity
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

  default fetchEndPoints
    :: (Functor m, MonadIO m, CoreEntityTable a)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]
  fetchEndPoints r t = case t of
    ToArtist -> fetch ArtistRelationship "artist"
    ToLabel -> fetch LabelRelationship "label"
    ToRecording -> fetch RecordingRelationship "recording"
    ToRelease -> fetch ReleaseRelationship "release"
    ToReleaseGroup -> fetch ReleaseGroupRelationship "release_group"
    ToUrl -> fetch UrlRelationship "url"
    ToWork -> fetch WorkRelationship "work"
   where
    source = untag (rootTable :: Tagged a String)
    fetch cons t1 =
      let q = fromString $ unlines
              [ "SELECT l." ++ t1 ++ "_id, l.relationship_id "
              , "FROM l_" ++ source ++ "_" ++ t1 ++ " l "
              , "JOIN " ++ source ++ "_tree source_tree ON (l." ++ source ++ "_tree_id = source_tree." ++ source ++ "_tree_id) "
              , "JOIN " ++ source ++ "_revision source ON (source." ++ source ++ "_tree_id = source_tree." ++ source ++ "_tree_id) "
              , "WHERE source.revision_id = ?"
              ]
        in map (constructPartialRel cons) <$> query q (Only r)
    constructPartialRel cons (targetId, relationshipId) =
      (cons targetId, relationshipId)

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
