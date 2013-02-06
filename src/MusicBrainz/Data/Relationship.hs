{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for working relationships of any general type. -}
module MusicBrainz.Data.Relationship
    (
      addRelationshipAttributeType

      -- * Viewing relationships
    , viewRelationships
    , HoldsRelationships
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mappend, mempty)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..), In(..), (:.)(..))
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
  add rt = do
    ((id', name, parent, childOrder, t0, t1, description) :. (linkPhrase, reverseLinkPhrase, shortLinkPhrase, priority)) <-
      head <$> query
        [sql|
          INSERT INTO link_type (name, parent, child_order, entity_type0,
            entity_type1, description, link_phrase, reverse_link_phrase,
            short_link_phrase, priority, gid)
          VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, uuid_generate_v4() )
          RETURNING id, name, parent, child_order, entity_type0, entity_type1,
            description, link_phrase, reverse_link_phrase, short_link_phrase,
            priority
        |] rt

    attrs <- returning
      [sql|
        INSERT INTO link_type_attribute_type
          (link_type, attribute_type, min, max)
        VALUES (?, ?, ?, ?)
        RETURNING attribute_type, min, max
      |] (map ((Only id') :.) $ Set.toList $ relTypeAttributes rt)

    return Entity
      { entityRef = id'
      , entityData =
          RelationshipType
            { relName = name
            , relTypeAttributes = Set.fromList attrs
            , relParent = parent
            , relChildOrder = childOrder
            , relLeftTarget = t0
            , relRightTarget = t1
            , relDescription = description
            , relLinkPhrase = linkPhrase
            , relReverseLinkPhrase = reverseLinkPhrase
            , relShortLinkPhrase = shortLinkPhrase
            , relPriority = priority
            }
      }


--------------------------------------------------------------------------------
instance ResolveReference RelationshipType where
  resolveReference relationshipTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM link_type WHERE id = ? |]
      (Only relationshipTypeId)


--------------------------------------------------------------------------------
instance ResolveReference RelationshipAttribute where
  resolveReference attributeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM link_attribute_type WHERE id = ? |]
      (Only attributeId)


--------------------------------------------------------------------------------
addRelationshipAttributeType :: (Functor m, MonadIO m)
  => Text
  -> Maybe (Ref RelationshipAttribute)
  -> Maybe (Ref RelationshipAttribute)
  -> Int
  -> Text
  -> MusicBrainzT m (Entity (RelationshipAttribute))
addRelationshipAttributeType name parent root childOrder description = do
    id' <- selectValue $ query_ [sql| SELECT nextval('link_attribute_type_id_seq') |]

    head <$>
      query [sql|
              INSERT INTO link_attribute_type
                (id, gid, parent, child_order, name, description, root)
              VALUES ( ?, uuid_generate_v4(), ?, ?, ?, ?, ?)
              RETURNING id, name, parent, root, child_order, description
            |] ( id' :: Int, parent, childOrder, name, description
               , fromMaybe (id' ^. reference) root
               )


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
