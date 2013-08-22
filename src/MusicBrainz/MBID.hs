{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.MBID (MBID(..), mbid) where

import Control.Applicative
import Control.Lens hiding (Action)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..), inQuotes)

import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString.Char8 as LBS
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple.FromField as FF

--------------------------------------------------------------------------------
{-| A MusicBrainz MBID, which is a 'UUID' but scoped to a specific entity
type. -}
newtype MBID a = MBID UUID
  deriving (Eq, Ord, Read, Show)

instance Wrapped UUID UUID (MBID a) (MBID a) where
  wrapped = iso MBID $ \(MBID a) -> a

instance FromField (MBID a) where
  fromField f v = MBID <$> uuidFromField f v

instance ToField (MBID a) where
  toField (MBID id') = uuidToField id'

uuidToField :: UUID -> Action
uuidToField = Plain . inQuotes . Blaze.fromString . UUID.toString

--------------------------------------------------------------------------------
uuidFromField :: FF.Field -> Maybe LBS.ByteString -> FF.Conversion UUID
uuidFromField f Nothing =
  FF.returnError FF.UnexpectedNull f "UUID cannot be null"

uuidFromField f (Just v) = do
  t <- FF.typename f
  if t /= "uuid"
      then incompatible
      else tryParse
  where
    incompatible =
      FF.returnError FF.Incompatible f "UUIDs must be PG type 'uuid'"

    tryParse = case UUID.fromString (LBS.unpack v) of
      Just uuid -> return uuid
      Nothing -> FF.returnError FF.ConversionFailed f "Not a valid UUID"


--------------------------------------------------------------------------------
{-| Inject a 'String' into an 'MBID', or extract a 'String' from an 'MBID'. To
work with this 'Prism', you should use '^?' to convert strings to MBIDs (there
is a chance of failure) and '^.' / 'remit' to extract the 'String' from an
'MBID':

> "10adbe5e-a2c0-4bf3-8249-2b4cbf6e6ca8" ^? mbid :: Maybe (MBID a)

> aValidMbidValue ^. remit mbid :: String
-}
mbid :: Prism' String (MBID a)
mbid = uuid . wrapped
 where
  uuid = prism UUID.toString parseUUID
   where parseUUID s = case UUID.fromString s of
           Just u -> Right u
           Nothing -> Left s
