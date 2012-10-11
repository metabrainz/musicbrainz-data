{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Schema () where

import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative
import Data.UUID (toString)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..), inQuotes)
import MusicBrainz.Types

--------------------------------------------------------------------------------
instance FromRow Artist where
  fromRow = Artist <$> field <*> field <*> field <*> fromRow
                   <*> fromRow <*> field <*> field <*> field
                   <*> field


instance FromRow PartialDate where
  fromRow = PartialDate <$> field <*> field <*> field


--------------------------------------------------------------------------------
instance FromField (Ref ArtistType) where
  fromField f v = ArtistTypeRef <$> fromField f v

instance FromField (Ref Country) where
  fromField f v = CountryRef <$> fromField f v

instance FromField (Ref Gender) where
  fromField f v = GenderRef <$> fromField f v

--------------------------------------------------------------------------------
instance ToField (MBID a) where
  toField (MBID mbid) = Plain $ inQuotes (fromString $ toString mbid)
