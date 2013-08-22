{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Editor where

import Control.Applicative
import Control.Lens
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Monad
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Entity
import MusicBrainz.Ref (Referenceable(..), Ref, reference, dereference)

--------------------------------------------------------------------------------
{-| A MusicBrainz editor who makes changes to the database. -}
data Editor = Editor { editorName :: !Text
                     , editorPassword :: !Text
                     }
  deriving (Eq, Show)

instance Referenceable Editor where
  type RefSpec Editor = Int

instance FromField (Ref Editor) where
  fromField f v = view reference <$> fromField f v

instance FromRow Editor where
  fromRow = Editor <$> field <*> field

instance ToField (Ref Editor) where
  toField = toField . dereference

instance ToRow Editor where
  toRow Editor{..} = [ toField editorName
                     , toField editorPassword
                     ]

instance ResolveReference Editor where
  resolveReference editorId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM editor WHERE id = ? |] (Only editorId)


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

