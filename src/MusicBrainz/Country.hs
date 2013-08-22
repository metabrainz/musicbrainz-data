{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Country (Country(..)) where

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
import MusicBrainz.Ref (Referenceable(..), Ref, dereference, reference)

--------------------------------------------------------------------------------
{-| A country where artists resides, labels are founded, releases are released
in, etc. -}
data Country = Country
    { countryIsoCode :: !Text
      -- ^ The ISO-3166 code for this country
    , countryName :: !Text
    }
  deriving (Eq, Show)

instance Referenceable Country where
  type RefSpec Country = Int

instance FromField (Ref Country) where
  fromField f v = view reference <$> fromField f v

instance FromRow Country where
  fromRow = Country <$> field <*> field

instance ToField (Ref Country) where
  toField = toField . dereference

instance ToRow Country where
  toRow Country{..} = [ toField countryIsoCode
                      , toField countryName
                      ]

instance Add Country where
  add country = head <$>
    query [sql| INSERT INTO country (iso_code, name) VALUES (?, ?)
                RETURNING id, iso_code, name |] country

instance ResolveReference Country where
  resolveReference countryId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM country WHERE id = ? |] (Only countryId)
