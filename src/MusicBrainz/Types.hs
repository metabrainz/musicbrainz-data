{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Types
    ( -- * MusicBrainz entities
      Artist(..)
    , ArtistType(..)
    , Country(..)
    , Gender(..)

      -- * Various types of data used in entity attributes
    , MBID(..)
    , parseMbid
    , PartialDate(..)

      -- * Entity/reference handling
    , Entity
    , Ref(..)
    ) where

import Data.Text (Text)
import Data.UUID

{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data family Ref a


{-| An 'Entity' is something that has been loaded from the database. It cotains
both data about itself (in @entityData@), and also a reference to itself (in
@entityRef@) so that other data/entities can refer to it. -}
data Entity a = Entity { entityData :: a
                       , entityRef :: Ref a
                       }


--------------------------------------------------------------------------------
{-| The data about an artist in MusicBrainz. -}
data Artist = Artist
    { artistName :: Text
    , artistSortName :: Text
    , artistComment :: Text
    , artistBeginDate :: PartialDate
    , artistEndDate :: PartialDate
    , artistEnded :: Bool
    , artistGender :: Maybe (Ref Gender)
    , artistType :: Maybe (Ref ArtistType)
    , artistCountry :: Maybe (Ref Country)
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
{-| The definition of a type of an artist (e.g., \"person\" or \"group\") . -}
data ArtistType = ArtistType
    { artistTypeName :: Text }
  deriving (Eq, Show)

data instance Ref ArtistType = ArtistTypeRef Int
deriving instance Eq (Ref ArtistType)
deriving instance Show (Ref ArtistType)


--------------------------------------------------------------------------------
data Country = Country
    { countryIsoCode :: Text
    , countryName :: Text
    }
  deriving (Eq, Show)

data instance Ref Country = CountryRef Int
deriving instance Eq (Ref Country)
deriving instance Show (Ref Country)


--------------------------------------------------------------------------------
{-| The gender of an artist or editor. -}
data Gender = Gender
    { genderName :: Text }
  deriving (Eq, Show)

data instance Ref Gender = GenderRef Int
deriving instance Eq (Ref Gender)
deriving instance Show (Ref Gender)


--------------------------------------------------------------------------------
{-| A partial date consisting of an optional year, month and day. -}
data PartialDate = PartialDate
    { dateYear :: Maybe Int
    , dateMonth :: Maybe Int
    , dateDay :: Maybe Int
     }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| A MusicBrainz MBID, which is a 'UUID' but scoped to a specific entity
type. -}
newtype MBID a = MBID UUID
  deriving (Eq, Show)

parseMbid :: String -> Maybe (MBID a)
parseMbid = fmap MBID . fromString
