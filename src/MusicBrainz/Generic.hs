{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module MusicBrainz.Generic
    ( realiseAliases
    , realiseIpiCodes
    , realiseIsniCodes
    , realiseRelationships
    , reflectRelationshipChange
    , addRelationship
    , findLatest
    ) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Lens hiding (cons)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.String (fromString)
import Data.Foldable (forM_)
import Database.PostgreSQL.Simple (In(..), Only(..), Query, (:.)(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Util (viewOnce)
import MusicBrainz.Monad
import MusicBrainz.Edit
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.Ref
import MusicBrainz.Relationship
import MusicBrainz.Tree

import MusicBrainz.Artist ()
import MusicBrainz.Label ()
import MusicBrainz.Recording ()
import MusicBrainz.Release ()
import MusicBrainz.ReleaseGroup ()
import MusicBrainz.URL ()
import MusicBrainz.Work ()

import qualified MusicBrainz.Class.FindLatest as MB

--------------------------------------------------------------------------------
realiseAliases :: (Functor m, MonadIO m, TreeAliases a) => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseAliases eName treeId tree = forM_ (Set.toList $ tree^.aliases) $ \alias ->
  execute q (Only treeId :. alias)
  where q = fromString $ unlines
          [ "INSERT INTO " ++ eName ++ "_alias (" ++ eName ++ "_tree_id, name, sort_name, "
          , "begin_date_year, begin_date_month, begin_date_day, "
          , "end_date_year, end_date_month, end_date_day, "
          , "ended, " ++ eName ++ "_alias_type_id, locale, primary_for_locale) "
          , "VALUES (?, (SELECT find_or_insert_" ++ eName ++ "_name(?)), "
          , "(SELECT find_or_insert_" ++ eName ++ "_name(?)), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ]


--------------------------------------------------------------------------------
realiseIpiCodes :: (Functor m, MonadIO m, TreeIPICodes a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseIpiCodes eName treeId tree = void $ executeMany q
      $ map (Only treeId :.) (Set.toList $ tree^.ipiCodes)
  where q = fromString $ "INSERT INTO " ++ eName ++ "_ipi (" ++ eName ++ "_tree_id, ipi) VALUES (?, ?)"


--------------------------------------------------------------------------------
realiseIsniCodes :: (Functor m, MonadIO m, TreeISNICodes a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseIsniCodes eName treeId tree = void $ executeMany q
      $ map (Only treeId :.) (Set.toList $ tree^.isniCodes)
  where q = fromString $ "INSERT INTO " ++ eName ++ "_Isni (" ++ eName ++ "_tree_id, isni) VALUES (?, ?)"


--------------------------------------------------------------------------------
reflectRelationshipChange :: (Ref a -> Relationship -> LinkedRelationship)
                          -> Ref Editor
                          -> Ref a
                          -> (LinkedRelationship -> Set.Set LinkedRelationship -> Set.Set LinkedRelationship)
                          -> LinkedRelationship
                          -> EditT ()
reflectRelationshipChange returnCon editor endpoint f toReflect =
  case toReflect of
    (ArtistRelationship targetId rel) -> reflect targetId rel
    (LabelRelationship targetId rel) -> reflect targetId rel
    (RecordingRelationship targetId rel) -> reflect targetId rel
    (ReleaseRelationship targetId rel) -> reflect targetId rel
    (ReleaseGroupRelationship targetId rel) -> reflect targetId rel
    (URLRelationship targetId rel) -> reflect targetId rel
    (WorkRelationship targetId rel) -> reflect targetId rel
  where
    reflect targetId rel = do
      let returnRelationship = returnCon endpoint rel
      target <- viewOnce MB.findLatest targetId
      targetTree <- over relationships (f returnRelationship) <$> viewTree (coreRevision target)
      void $ runUpdate editor (coreRevision target) targetTree


--------------------------------------------------------------------------------
addRelationship :: (Functor m, MonadIO m)
  => String -> Ref (Tree a) -> LinkedRelationship -> MusicBrainzT m ()
addRelationship source treeId rel = case rel of
    (ArtistRelationship targetId relInfo) -> go "artist" targetId relInfo
    (LabelRelationship targetId relInfo) -> go "label" targetId relInfo
    (RecordingRelationship targetId relInfo) -> go "recording" targetId relInfo
    (ReleaseRelationship targetId relInfo) -> go "release" targetId relInfo
    (ReleaseGroupRelationship targetId relInfo) -> go "release_group" targetId relInfo
    (WorkRelationship targetId relInfo) -> go "work" targetId relInfo
    (URLRelationship targetId relInfo) -> go "url" targetId relInfo
  where
    go target targetId relInfo = do
      relationshipId <- selectValue $ query [sql|
        INSERT INTO link (link_type,
          begin_date_year, begin_date_month, begin_date_day,
          end_date_year, end_date_month, end_date_day,
          ended)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id |] relInfo
      executeMany [sql| INSERT INTO link_attribute (link, attribute_type) VALUES (?, ?) |]
        (map (relationshipId, ) $ Set.toList $ relAttributes relInfo)
      let q = fromString $ unlines
            [ "INSERT INTO l_" ++ source ++ "_" ++ target ++ " "
            , "(" ++ source ++ "_tree_id, " ++ target ++ "_id, relationship_id) "
            , "VALUES (?, ?, ?)"
            ]
      void $ execute q (treeId, targetId, relationshipId :: Int)


--------------------------------------------------------------------------------
realiseRelationships :: (Functor m, MonadIO m, TreeRelationships a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseRelationships tbl treeId =
  mapMOf_ (relationships.folded) (addRelationship tbl treeId)

--------------------------------------------------------------------------------
findLatest :: (MB.FindLatest a, Functor m, MonadIO m, ToField (Ref a), FromRow a, FromField (Ref a))
  => Query -> Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))
findLatest q = fmap toMap . query q . Only . In . Set.toList
  where toMap = Map.fromList . map (coreRef &&& id)
