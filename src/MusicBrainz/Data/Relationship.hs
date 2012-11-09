{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Relationship
    ( addRelationshipType ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

--------------------------------------------------------------------------------
{-| Add a new 'RelationshipType' to the list of known relationship types in
MusicBrainz. -}
addRelationshipType :: RelationshipType -> MusicBrainz (Entity RelationshipType)
addRelationshipType rt = head <$>
  query [sql| INSERT INTO relationship_type (name) VALUES (?)
              RETURNING relationship_type_id, name |] rt



