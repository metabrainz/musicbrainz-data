{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MusicBrainz.Class.ResolveReference where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.MBID (MBID)
import MusicBrainz.Ref (Ref, RefSpec)
import MusicBrainz.Revision (Revision)

--------------------------------------------------------------------------------
class ResolveReference a where
  {-| Attempt to resolve a reference from its attributes. If the attributes
  don't actually correspond to an entity in the database, then 'Nothing' is
  returned. -}
  resolveReference
    :: (Functor m, MonadIO m) => RefSpec a -> MusicBrainzT m (Maybe (Ref a))

  default resolveReference
    :: (Functor m, MonadIO m, GenericResolver a (RefSpec a))
    => RefSpec a -> MusicBrainzT m (Maybe (Ref a))
  resolveReference = genericResolveReference


--------------------------------------------------------------------------------
class RefSpec a ~ r => GenericResolver a r | r -> a where
  genericResolveReference
    :: (Functor m, MonadIO m)
    => r -> MusicBrainzT m (Maybe (Ref a))

instance (RootTable a, FromField (Ref a), RefSpec a ~ MBID a) => GenericResolver a (MBID a) where
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

instance RootTable a => GenericResolver (Revision a) Int where
  genericResolveReference revisionId =
    listToMaybe . map fromOnly <$> query q (Only revisionId)
   where
    eName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "SELECT revision_id "
        , "FROM " ++ eName ++ "_revision "
        , "WHERE revision_id = ?"
        ]
