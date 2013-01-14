{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with 'Editor's in the MusicBrainz database. -}
module MusicBrainz.Data.Editor
    ( -- * Finding editors
      findEditorByName

      -- * Creating new editors
    , register
    ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
{-| Look up an editor by their name. -}
findEditorByName :: Text -> MusicBrainz (Maybe (Entity Editor))
findEditorByName name =
  listToMaybe <$> query [sql| SELECT id, name, password FROM editor WHERE name = ? |]
             (Only name)


--------------------------------------------------------------------------------
{-| Register a new MusicBrainz editor. -}
register :: Editor -> MusicBrainz (Entity Editor)
register editor = head <$> query
  [sql| INSERT INTO editor (name, password) VALUES (?, ?)
        RETURNING id, name, password |] editor


--------------------------------------------------------------------------------
instance ResolveReference Editor where
  resolveReference editorId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM editor WHERE id = ? |] (Only editorId)
