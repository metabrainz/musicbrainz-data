{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'AliasType's. -}
module MusicBrainz.Data.AliasType ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz hiding (aliasType)
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add (AliasType Artist) where
  add type' = head <$>
    query [sql| INSERT INTO artist_alias_type (name) VALUES (?)
                RETURNING id, name |] type'


--------------------------------------------------------------------------------
instance ResolveReference (AliasType Artist) where
  resolveReference aliasTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM artist_alias_type WHERE id = ? |] (Only aliasTypeId)


--------------------------------------------------------------------------------
instance Add (AliasType Label) where
  add type' = head <$>
    query [sql| INSERT INTO label_alias_type (name) VALUES (?)
                RETURNING id, name |] type'


--------------------------------------------------------------------------------
instance ResolveReference (AliasType Label) where
  resolveReference aliasTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM label_alias_type WHERE id = ? |] (Only aliasTypeId)


--------------------------------------------------------------------------------
instance Add (AliasType Work) where
  add type' = head <$>
    query [sql| INSERT INTO work_alias_type (name) VALUES (?)
                RETURNING id, name |] type'


--------------------------------------------------------------------------------
instance ResolveReference (AliasType Work) where
  resolveReference aliasTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM work_alias_type WHERE id = ? |] (Only aliasTypeId)
