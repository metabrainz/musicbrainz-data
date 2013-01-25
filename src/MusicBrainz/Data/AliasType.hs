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
import MusicBrainz.Types.Internal

--------------------------------------------------------------------------------
instance Add (AliasType WorkAlias) where
  add type' = head <$>
    query [sql| INSERT INTO work_alias_type (name) VALUES (?)
                RETURNING id, name |] type'


--------------------------------------------------------------------------------
instance ResolveReference (AliasType WorkAlias) where
  resolveReference aliasTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM work_alias_type WHERE id = ? |] (Only aliasTypeId)
