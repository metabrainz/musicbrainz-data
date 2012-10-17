{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-| Definitions of all types used within MusicBrainz. -}
module MusicBrainz.Types
    ( -- * MusicBrainz entities
      Artist(..)
    , ArtistType(..)
    , Country(..)
    , Editor(..)
    , Gender(..)

      -- * Various types of data used in entity attributes
    , MBID(..)
    , parseMbid, mbidToString
    , PartialDate(..)
    , emptyDate, isEmpty

      -- * Versioning
    , CoreEntity(..)
    , Revision

      -- * Entity/reference handling
    , Entity(..)
    , Ref(..)
    ) where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID

{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data family Ref a


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
  deriving (Eq, Show, Typeable)

--------------------------------------------------------------------------------
{-| The definition of a type of an artist (e.g., \"person\" or \"group\") . -}
data ArtistType = ArtistType
    { artistTypeName :: Text }
  deriving (Eq, Show)

data instance Ref ArtistType = ArtistTypeRef Int
deriving instance Eq (Ref ArtistType)
deriving instance Show (Ref ArtistType)


--------------------------------------------------------------------------------
{-| A country where artists resides, labels are founded, releases are released
in, etc. -}
data Country = Country
    { countryIsoCode :: Text
      -- ^ The ISO-3166 code for this country
    , countryName :: Text
    }
  deriving (Eq, Show)

data instance Ref Country = CountryRef Int
deriving instance Eq (Ref Country)
deriving instance Show (Ref Country)


--------------------------------------------------------------------------------
{-| A MusicBrainz editor who makes changes to the database. -}
data Editor = Editor { editorName :: Text }

data instance Ref Editor = EditorRef Int


--------------------------------------------------------------------------------
{-| An 'Entity' is something that has been loaded from the database. It cotains
both data about itself (in @entityData@), and also a reference to itself (in
@entityRef@) so that other data/entities can refer to it. -}
data Entity a = Entity { entityRef :: Ref a
                       , entityData :: a
                       }


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


{-| A 'PartialDate' with no year, month or day. -}
emptyDate :: PartialDate
emptyDate = PartialDate Nothing Nothing Nothing


isEmpty :: PartialDate -> Bool
isEmpty = (== emptyDate)

--------------------------------------------------------------------------------
{-| A MusicBrainz MBID, which is a 'UUID' but scoped to a specific entity
type. -}
newtype MBID a = MBID UUID
  deriving (Eq, Show, Typeable)


{-| Attempt to parse an 'MBID' from a 'String'. If parsing fails, 'Nothing'
is returned. -}
parseMbid :: String -> Maybe (MBID a)
parseMbid = fmap MBID . fromString


{-| Convert an 'MBID' to a 'String'. -}
mbidToString :: MBID a -> String
mbidToString (MBID m) = toString m

--------------------------------------------------------------------------------
{-| Represents a view of a versioned MusicBrainz \'core\' entity at a specific
point in time (a specific 'Revision'). -}
data CoreEntity a = CoreEntity
    { coreMbid :: MBID a
    , coreRevision :: Ref (Revision a)
    , coreData :: a
    }

deriving instance (Eq a, Show a) => Eq (CoreEntity a)
deriving instance (Eq a, Show a) => Show (CoreEntity a)

--------------------------------------------------------------------------------
{-| A revision is a version of an entity at a specific point in time. The type
@a@ indicates what type of entity this is a revision of (e.g., @Revision Artist@
means a specific revision of an 'Artist'). -}
data Revision a

data instance Ref (Revision a) = RevisionRef Int
deriving instance Eq (Ref (Revision a))
deriving instance Show (Ref (Revision a))
