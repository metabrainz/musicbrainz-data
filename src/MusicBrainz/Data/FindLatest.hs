{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Provides the 'FindLatest' type class. -}
module MusicBrainz.Data.FindLatest
    ( FindLatest(..)
    , ResolveReference(..)
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Maybe (listToMaybe)
import Data.Tagged (Tagged, untag)
import Data.String (fromString)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Data.CoreEntity
import MusicBrainz.Monad
import MusicBrainz.Types

--------------------------------------------------------------------------------
{-| Attempt to find the latest revision of an entity (type @a@), by a given
'Ref'. To obtain the reference, you can use
'MusicBrainz.Data.Merge.resolveMbid'. -}
class FindLatest a where
  findLatest :: (Applicative m, Functor m, MonadIO m) =>
    Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))


--------------------------------------------------------------------------------
class ResolveReference a where
  {-| Attempt to resolve a reference from its attributes. If the attributes
  don't actually correspond to an entity in the database, then 'Nothing' is
  returned. -}
  resolveReference :: (Functor m, MonadIO m) => RefSpec a -> MusicBrainzT m (Maybe (Ref a))

  default resolveReference
    :: (Functor m, MonadIO m, GenericResolver a)
    => RefSpec a -> MusicBrainzT m (Maybe (Ref a))
  resolveReference = genericResolveReference

class GenericResolver a where
  genericResolveReference
    :: (Functor m, MonadIO m)
    => RefSpec a -> MusicBrainzT m (Maybe (Ref a))

instance (RefSpec a ~ MBID a, CoreEntityTable a, FromField (Ref a), ToField (MBID a)) => GenericResolver a where
  genericResolveReference entityMbid =
    listToMaybe . map fromOnly <$> query q (Only entityMbid)
   where
    eName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "WITH RECURSIVE path (revision_id, " ++ eName ++ "_id, child_revision_id, created_at, is_master_revision_id) "
        , "AS ("
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id AS child_revision_id, "
        , "    created_at, "
        , "    TRUE as is_master_revision_id "
        , "  FROM " ++ eName ++ "_revision "
        , "  JOIN " ++ eName ++ " USING (" ++ eName ++ "_id) "
        , "  JOIN revision USING (revision_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = revision.revision_id) "
        , "  WHERE " ++ eName ++ "_id = ? AND master_revision_id = " ++ eName ++ "_revision.revision_id "
        , " "
        , "  UNION "
        , " "
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id, "
        , "    revision.created_at, "
        , "    master_revision_id = " ++ eName ++ "_revision.revision_id AS is_master_revision_id "
        , "  FROM path "
        , "  JOIN " ++ eName ++ "_revision ON (path.child_revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN revision ON (revision.revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN " ++ eName ++ " ON (" ++ eName ++ "." ++ eName ++ "_id = " ++ eName ++ "_revision." ++ eName ++ "_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = " ++ eName ++ "_revision.revision_id) "
        , ") "
        , "SELECT " ++ eName ++ "_id "
        , "FROM path "
        , "WHERE is_master_revision_id "
        , "ORDER BY created_at DESC, revision_id DESC "
        , "LIMIT 1 "
        ]

instance (CoreEntityTable a, FromField (Ref (Revision a))) => GenericResolver (Revision a) where
  genericResolveReference revisionId =
    listToMaybe . map fromOnly <$> query q (Only revisionId)
   where
    eName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "SELECT revision_id "
        , "FROM " ++ eName ++ "_revision "
        , "WHERE revision_id = ?"
        ]

