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
    , ArtistCredit
    , ArtistType(..)
    , Country(..)
    , Editor(..)
    , Gender(..)
    , Label(..)
    , LabelType(..)
    , Language
    , Recording(..)
    , Release(..)
    , ReleaseGroup(..)
    , ReleaseGroupType(..)
    , ReleasePackaging
    , ReleaseStatus
    , Script

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
{-| An artist credit is an ordered lists of artists, intercalated with free text
strings and with each artist having a a credited name (possibly different from
the actual artists name. -}
data ArtistCredit

data instance Ref ArtistCredit = ArtistCreditRef Int
deriving instance Eq (Ref ArtistCredit)
deriving instance Show (Ref ArtistCredit)

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
{-| A label who is repsonsible for releasing/distributing music. -}
data Label = Label { labelName :: Text
                   , labelSortName :: Text
                   , labelComment :: Text
                   , labelBeginDate :: PartialDate
                   , labelEndDate :: PartialDate
                   , labelEnded :: Bool
                   , labelType :: Maybe (Ref LabelType)
                   , labelCode :: Maybe Integer
                   }
  deriving (Eq, Show, Typeable)


--------------------------------------------------------------------------------
{-| The definition of a type of an label (e.g., \"person\" or \"group\") . -}
data LabelType = LabelType
    { labelTypeName :: Text }
  deriving (Eq, Show)

data instance Ref LabelType = LabelTypeRef Int
deriving instance Eq (Ref LabelType)
deriving instance Show (Ref LabelType)


--------------------------------------------------------------------------------
{-| A language that is written or spoken. -}
data Language

data instance Ref Language = LanguageRef Int
deriving instance Eq (Ref Language)
deriving instance Show (Ref Language)

--------------------------------------------------------------------------------
{-| A recording in MusicBrainz (which is realised on 'Tracklist' as a
'Track'. -}
data Recording = Recording
    { recordingName :: Text
    , recordingComment :: Text
    , recordingArtistCredit :: Ref (ArtistCredit)
    , recordingDuration :: Int
    }
  deriving (Eq, Show, Typeable)

data instance Ref Recording = RecordingRef Int
deriving instance Eq (Ref Recording)
deriving instance Show (Ref Recording)


--------------------------------------------------------------------------------
{-| A release in MusicBrainz is a physical product that people can purchase,
and belongs to a 'ReleaseGroup' and consists of some information along with
multiple 'Medium's. -}
data Release = Release
    { releaseName :: Text
    , releaseComment :: Text
    , releaseArtistCredit :: Ref (ArtistCredit)
    , releaseReleaseGroup :: Ref (ReleaseGroup)
    , releaseDate :: PartialDate
    , releaseCountry :: Maybe (Ref Country)
    , releaseScript :: Maybe (Ref Script)
    , releaseLanguage :: Maybe (Ref Language)
    , releasePackaging :: Maybe (Ref ReleasePackaging)
    , releaseStatus :: Maybe (Ref ReleaseStatus)
    }
  deriving (Eq, Show, Typeable)

data instance Ref Release = ReleaseRef Int
deriving instance Eq (Ref Release)
deriving instance Show (Ref Release)


--------------------------------------------------------------------------------
{-| A release group is an abstract MusicBrainz concept which groups multiple
'Release's logically together. For example, a release group might contain the
various different formats of albums, such as the vinyl release and the CD
release. -}
data ReleaseGroup = ReleaseGroup
    { releaseGroupName :: Text
    , releaseGroupComment :: Text
    , releaseGroupArtistCredit :: Ref ArtistCredit
    , releaseGroupPrimaryType :: Maybe (Ref (ReleaseGroupType Primary))
    }
  deriving (Eq, Show, Typeable)

data instance Ref ReleaseGroup = ReleaseGroupRef (MBID ReleaseGroup)
deriving instance Eq (Ref ReleaseGroup)
deriving instance Show (Ref ReleaseGroup)


--------------------------------------------------------------------------------
data Primary

{-| A release group type indicates the various types a release group can be.
For example, one release group type combination might be 'Album + Remix' to
indicate a remix album.

The parameter to 'ReleaseGroupType' indicates whether the release group
type is primary or secondary (it is kinded as 'ReleaseGroupTypeClass'. -}
data ReleaseGroupType a = ReleaseGroupType
    { releaseGroupTypeName :: Text }
  deriving (Eq, Show)

data instance Ref (ReleaseGroupType a) = ReleaseGroupTypeRef Int
deriving instance Eq (Ref (ReleaseGroupType a))
deriving instance Show (Ref (ReleaseGroupType a))


--------------------------------------------------------------------------------
{-| The type of packaging a release came in. -}
data ReleasePackaging

data instance Ref ReleasePackaging = ReleasePackagingRef Int
deriving instance Eq (Ref ReleasePackaging)
deriving instance Show (Ref ReleasePackaging)


--------------------------------------------------------------------------------
{-| A release status indicates whether a 'Release' was released official,
promotionally, as a bootleg, etc. -}
data ReleaseStatus

data instance Ref ReleaseStatus = ReleaseStatusRef Int
deriving instance Eq (Ref ReleaseStatus)
deriving instance Show (Ref ReleaseStatus)


--------------------------------------------------------------------------------
{-| The script that text is written. -}
data Script

data instance Ref Script = ScriptRef Int
deriving instance Eq (Ref Script)
deriving instance Show (Ref Script)


--------------------------------------------------------------------------------
{-| A partial date consisting of an optional year, month and day.-}
data PartialDate = PartialDate
    { dateYear :: Maybe Int
    , dateMonth :: Maybe Int
    , dateDay :: Maybe Int
    }
  deriving (Eq, Show)


{-| A 'PartialDate' with no year, month or day. -}
emptyDate :: PartialDate
emptyDate = PartialDate Nothing Nothing Nothing


{-| Determinate if a 'PartialDate' is the empty date (with no fields set). -}
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
