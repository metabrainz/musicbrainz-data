{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Language where

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
import MusicBrainz.Entity (Add(..))
import MusicBrainz.Ref (Referenceable(..), Ref, reference, dereference)

--------------------------------------------------------------------------------
{-| A language that is written or spoken. -}
data Language = Language { languageName :: !Text
                         , languageIsoCode2t :: !Text
                         , languageIsoCode2b :: !Text
                         , languageIsoCode1 :: !Text
                         , languageIsoCode3  :: !Text
                         }
  deriving (Eq, Show)

instance Referenceable Language where
  type RefSpec Language = Int

instance FromField (Ref Language) where
  fromField f v = view reference <$> fromField f v

instance FromRow Language where
  fromRow = Language <$> field <*> field <*> field <*> field <*> field

instance ToField (Ref Language) where
  toField = toField . dereference

instance ToRow Language where
  toRow Language{..} = [ toField languageName
                       , toField languageIsoCode2t
                       , toField languageIsoCode2b
                       , toField languageIsoCode1
                       , toField languageIsoCode3
                       ]

instance Add Language where
  add language = head <$>
    query [sql| INSERT INTO language (name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3) VALUES (?, ?, ?, ?, ?)
                RETURNING id, name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3 |] language

instance ResolveReference Language where
  resolveReference languageId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM language WHERE id = ? |] (Only languageId)
