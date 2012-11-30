{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for working relationships of any general type. -}
module MusicBrainz.Data.Relationship
    ( -- * Viewing relationships
      viewRelationships
    , HoldsRelationships
    ) where

import Control.Applicative
import Control.Lens hiding (by)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..), In(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Types.Internal

--------------------------------------------------------------------------------
instance Add RelationshipType where
  add rt = head <$>
    query [sql| INSERT INTO relationship_type (name) VALUES (?)
                RETURNING relationship_type_id, name |] rt


--------------------------------------------------------------------------------
inflateRelationships :: (Functor m, MonadIO m) => [Int] -> MusicBrainzT m (Map.Map Int Relationship)
inflateRelationships relationshipIds = do
  attrs <- Map.fromListWith (Set.union) . over (mapped._2) Set.singleton
    <$> allAttributes

  relRows <- query [sql|
      SELECT relationship_id, relationship_type_id,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended
      FROM relationship
      WHERE relationship_id IN ?
    |] (Only $ In relationshipIds)

  return $ Map.fromList $ map (constructRelationship attrs) relRows

  where
    allAttributes :: MonadIO m => MusicBrainzT m [(Int, Ref RelationshipAttribute)]
    allAttributes = query [sql|
      SELECT relationship_id, attribute_type_id
      FROM relationship_attribute
      WHERE relationship_id IN ?
    |] (Only $ In relationshipIds)

    constructRelationship attrMap
      (relId, typeId, by, bm, bd, ey, em, ed, ended) =
        let relationship = Relationship
              { relType = typeId
              , relAttributes = Map.findWithDefault Set.empty relId attrMap
              , relBeginDate = PartialDate by bm bd
              , relEndDate = PartialDate ey em ed
              , relEnded = ended
              }
        in (relId, relationship)


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
