{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for working relationships of any general type. -}
module MusicBrainz.Data.Relationship
    ( -- * Viewing relationships
      viewRelationships
    , HoldsRelationships
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Monoid (mappend, mempty)
import Database.PostgreSQL.Simple (Only(..), In(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Add
import {-# SOURCE #-} MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.FindLatest
import MusicBrainz.Types.Internal

--------------------------------------------------------------------------------
instance Add RelationshipType where
  add type' = head <$>
    query [sql| INSERT INTO link_type (entity_type0, entity_type1, name,
                    description, link_phrase, reverse_link_phrase,
                    short_link_phrase)
                VALUES ( 'undefined: Add RelationshipType'
                       , 'undefined: Add RelationshipType'
                       , ?
                       , 'undefined: Add RelationshipType'
                       , 'undefined: Add RelationshipType'
                       , 'undefined: Add RelationshipType'
                       , 'undefined: Add RelationshipType'
                       , 'undefined: Add RelationshipType'
                       )
                RETURNING id, name |] type'


--------------------------------------------------------------------------------
instance ResolveReference RelationshipType where
  resolveReference relationshipTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM link_type WHERE id = ? |]
      (Only relationshipTypeId)


--------------------------------------------------------------------------------
inflateRelationships :: (Functor m, MonadIO m) => [Int] -> MusicBrainzT m (Map.Map Int Relationship)
inflateRelationships relationshipIds = do
  attrs <- Map.fromListWith mappend . over (mapped._2) Set.singleton
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
              , relAttributes = Map.findWithDefault mempty relId attrMap
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
