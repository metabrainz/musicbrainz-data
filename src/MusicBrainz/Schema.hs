{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-| Contains instances of 'FromField', 'FromRow', 'ToField' and 'ToRow' to
serialize to the MusicBrainz PostgreSQL database, and retrieve data from
it. This module doesn't really export much, but importing it will bring in
many type classes instances. 'MusicBrainz' itself re-exports this module,
so you should usually import that.-}
module MusicBrainz.Schema () where

import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..), inQuotes)
import MusicBrainz.Types

import qualified Data.ByteString.Char8 as LBS
import qualified Data.UUID as UUID

--------------------------------------------------------------------------------
instance (FromRow a, Typeable a) => FromRow (CoreEntity a) where
  fromRow = CoreEntity     -- Core entity's MBID
                       <$> field
                           -- The revision reference
                       <*> field
                           -- Delegetate to the actual entity to parse its data.
                       <*> fromRow



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


instance Typeable a => FromField (MBID a) where
  fromField f Nothing = returnError UnexpectedNull f "MBID cannot be null"
  fromField f (Just v) = case UUID.fromString (LBS.unpack v) of
    Just uuid -> return $ MBID uuid
    Nothing -> returnError ConversionFailed f "Not a valid MBID"

instance FromField (Ref (Revision a)) where
  fromField f v = RevisionRef <$> fromField f v

--------------------------------------------------------------------------------
instance ToField (MBID a) where
  toField (MBID mbid) = Plain $ inQuotes (fromString $ UUID.toString mbid)


instance ToField (Ref (Revision a)) where
  toField (RevisionRef id') = toField id'
