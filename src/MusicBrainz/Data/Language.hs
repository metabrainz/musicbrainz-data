{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Language's. -}
module MusicBrainz.Data.Language ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add Language where
  add language = head <$>
    query [sql| INSERT INTO language (name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3) VALUES (?, ?, ?, ?, ?)
                RETURNING id, name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3 |] language


--------------------------------------------------------------------------------
instance ResolveReference Language where
  resolveReference languageId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM language WHERE id = ? |] (Only languageId)
