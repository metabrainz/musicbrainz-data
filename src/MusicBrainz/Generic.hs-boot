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

import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Monad
import MusicBrainz.Edit
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.Ref
import MusicBrainz.Relationship
import MusicBrainz.Tree

import qualified MusicBrainz.Class.FindLatest as MB

realiseAliases :: (Functor m, MonadIO m, TreeAliases a) => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()

realiseIpiCodes :: (Functor m, MonadIO m, TreeIPICodes a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()

realiseIsniCodes :: (Functor m, MonadIO m, TreeISNICodes a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()

reflectRelationshipChange :: (Ref a -> Relationship -> LinkedRelationship)
                          -> Ref Editor
                          -> Ref a
                          -> (LinkedRelationship -> Set.Set LinkedRelationship -> Set.Set LinkedRelationship)
                          -> LinkedRelationship
                          -> EditT ()

addRelationship :: (Functor m, MonadIO m)
  => String -> Ref (Tree a) -> LinkedRelationship -> MusicBrainzT m ()

realiseRelationships :: (Functor m, MonadIO m, TreeRelationships a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()

findLatest :: (MB.FindLatest a, Functor m, MonadIO m, ToField (Ref a), FromRow a, FromField (Ref a))
  => Query -> Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))
