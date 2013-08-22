{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Relationship.Internal
    ( HoldsRelationships(..)
    , reflectRelationshipChanges
    , viewRelationships
    ) where

import Control.Applicative
import Control.Lens hiding (cons)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (mappend, mempty)
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Foldable (forM_)
import Data.Set (Set)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.PartialDate
import MusicBrainz.Ref (Ref)
import MusicBrainz.Revision (Revision)
import {-# SOURCE #-} MusicBrainz.Tree (Tree, TreeRelationships, relationships)
import MusicBrainz.Relationship

import {-# SOURCE #-} MusicBrainz.Artist ()

--------------------------------------------------------------------------------
{-| This type class is implemented for any entities that may have
'Relationship's. -}
class HoldsRelationships a where
  fetchEndPoints :: (Functor m, MonadIO m)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]

  default fetchEndPoints
    :: (Functor m, MonadIO m, RootTable a)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]
  fetchEndPoints r t = case t of
    ToArtist -> fetch ArtistRelationship "artist"
    ToLabel -> fetch LabelRelationship "label"
    ToRecording -> fetch RecordingRelationship "recording"
    ToRelease -> fetch ReleaseRelationship "release"
    ToReleaseGroup -> fetch ReleaseGroupRelationship "release_group"
    ToURL -> fetch URLRelationship "url"
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
    Ref Editor -> Ref a -> (LinkedRelationship -> Set LinkedRelationship -> Set LinkedRelationship) -> LinkedRelationship -> EditT ()


--------------------------------------------------------------------------------
-- Reflect relationship changes against other entities
reflectRelationshipChanges :: (ViewRevision a, HoldsRelationships a, TreeRelationships a)
  => Ref Editor
  -> Ref (Revision a)
  -> Tree a
  -> EditT ()
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

--------------------------------------------------------------------------------
{-| Find all relationships of a specific 'Revision' of an entity that can
have relationships. -}
viewRelationships :: (Functor m, HoldsRelationships a, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set LinkedRelationship)
viewRelationships r = do
  allRels <- concat <$>
    mapM (fetchEndPoints r) [minBound :: RelationshipTarget ..]
  inflatedRels <- inflateRelationships (map snd allRels)
  return $ Set.fromList $ map (construct inflatedRels) allRels

  where
    construct inflatedRels (f, relationshipId) =
      f (inflatedRels Map.! relationshipId)


--------------------------------------------------------------------------------
inflateRelationships :: (Functor m, MonadIO m) => [Int] -> MusicBrainzT m (Map.Map Int Relationship)
inflateRelationships relationshipIds = do
  attrs <- Map.fromListWith mappend . over (mapped._2) Set.singleton
    <$> allAttributes

  relRows <- query [sql|
      SELECT id, link_type,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended
      FROM link
      WHERE id IN ?
    |] (Only $ In relationshipIds)

  return $ Map.fromList $ map (constructRelationship attrs) relRows

  where
    allAttributes :: MonadIO m => MusicBrainzT m [(Int, Ref RelationshipAttribute)]
    allAttributes = query [sql|
      SELECT link, attribute_type
      FROM link_attribute
      WHERE link IN ?
    |] (Only $ In relationshipIds)

    constructRelationship attrMap
      (relId, typeId, by, bm, bd, ey, em, ed, ended) =
        let relationship = Relationship
              { relType = typeId
              , relAttributes = Map.findWithDefault mempty relId attrMap
              , relBeginDate = (by, bm, bd) ^?! partialDate
              , relEndDate = (ey, em, ed) ^?! partialDate
              , relEnded = ended
              }
        in (relId, relationship)

