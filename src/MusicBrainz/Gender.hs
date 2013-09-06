{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Gender (Gender(..)) where

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
{-| The gender of an artist or editor. -}
newtype Gender = Gender { genderName :: Text }
  deriving (Eq, Show)

instance Referenceable Gender where
  type RefSpec Gender = Int

instance FromField (Ref Gender) where
  fromField f v = view reference <$> fromField f v

instance FromRow Gender where
  fromRow = Gender <$> field

instance ToField (Ref Gender) where
  toField = toField . dereference

instance ToRow Gender where
  toRow Gender{..} = [ toField genderName
                     ]

instance Add Gender where
  add gender = head <$>
    query [sql| INSERT INTO gender (name) VALUES (?)
                RETURNING id, name |] gender

instance ResolveReference Gender where
  resolveReference genderId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM gender WHERE id = ? |] (Only genderId)
