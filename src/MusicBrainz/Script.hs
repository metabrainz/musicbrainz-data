{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Script where

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
import MusicBrainz.Class.Add
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)

--------------------------------------------------------------------------------
{-| The script that text is written. -}
data Script = Script { scriptIsoCode :: !Text
                     , scriptIsoNumber :: !Text
                     , scriptName :: !Text
                     }
  deriving (Eq, Show)

instance Referenceable Script where
  type RefSpec Script = Int

instance FromField (Ref Script) where
  fromField f v = view reference <$> fromField f v

instance FromRow Script where
  fromRow = Script <$> field <*> field <*> field

instance ToField (Ref Script) where
  toField = toField . dereference

instance ToRow Script where
  toRow Script{..} = [ toField scriptIsoCode
                     , toField scriptIsoNumber
                     , toField scriptName
                     ]

instance Add Script where
  add script = head <$>
    query [sql| INSERT INTO script (iso_code, iso_number, name) VALUES (?, ?, ?)
                RETURNING id, iso_code, iso_number, name |] script

instance ResolveReference Script where
  resolveReference scriptRef = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM script WHERE id = ? |] (Only scriptRef)
