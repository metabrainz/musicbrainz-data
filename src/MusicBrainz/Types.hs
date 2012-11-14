{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-| Definitions of all types used within MusicBrainz. -}
module MusicBrainz.Types
    ( -- * MusicBrainz entities
      Alias(..)
    , AliasType(..)
    , Artist(..)
    , ArtistCredit
    , ArtistCreditName(..)
    , ArtistType(..)
    , Country(..)
    , Editor(..)
    , Gender(..)
    , IPI(..)
    , Label(..)
    , LabelType(..)
    , Language(..)
    , Recording(..)
    , Release(..)
    , ReleaseGroup(..)
    , ReleaseGroupType(..), Primary
    , ReleasePackaging(..)
    , ReleaseStatus(..)
    , Script(..)

      -- ** Relationships
    , LinkedRelationship(..)
    , Relationship(..)
    , RelationshipType(..)
    , RelationshipAttribute(..)

      -- * Various types of data used in entity attributes
    , MBID(..), mbid
    , PartialDate(..)
    , emptyDate, isEmpty

      -- * Versioning
    , CoreEntity(..)
    , Revision
    , Tree(..)
    , treeData

      -- ** Edit system mechanics
    , Edit(..)
    , EditNote(..)
    , Vote(..)
    , EditStatus(..)

      -- * Entity/reference handling
    , Entity(..)
    , Ref(..)
    ) where

import Control.Lens
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID
import GHC.Enum (boundedEnumFrom)

import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data Ref a where
    AliasTypeRef :: Int -> Ref AliasType
    ArtistCreditRef :: Int -> Ref ArtistCredit
    ArtistRef :: (MBID Artist) -> Ref Artist
    ArtistTypeRef :: Int -> Ref ArtistType
    CountryRef :: Int -> Ref Country
    EditRef :: Int -> Ref Edit
    EditNoteRef :: Int -> Ref EditNote
    EditorRef :: Int -> Ref Editor
    GenderRef :: Int -> Ref Gender
    LabelRef :: (MBID Label) -> Ref Label
    LabelTypeRef :: Int -> Ref LabelType
    LanguageRef :: Int -> Ref Language
    RecordingRef :: (MBID Recording) -> Ref Recording
    RelationshipAttributeRef :: Int -> Ref RelationshipAttribute
    RelationshipTypeRef :: Int -> Ref RelationshipType
    ReleaseRef :: (MBID Release) -> Ref Release
    ReleaseGroupRef :: (MBID ReleaseGroup) -> Ref ReleaseGroup
    ReleaseGroupTypeRef :: Int -> Ref (ReleaseGroupType a)
    ReleasePackagingRef :: Int -> Ref ReleasePackaging
    ReleaseStatusRef :: Int -> Ref ReleaseStatus
    ScriptRef :: Int -> Ref Script
    RevisionRef :: Int -> Ref (Revision a)
    TreeRef :: Int -> Ref (Tree a)

deriving instance Eq (Ref a)
deriving instance Show (Ref a)

instance Ord (Ref a) where
  compare (ArtistCreditRef a) (ArtistCreditRef b) = a `compare` b
  compare (ArtistRef a) (ArtistRef b) = a `compare` b
  compare (ArtistTypeRef a) (ArtistTypeRef b) = a `compare` b
  compare (CountryRef a) (CountryRef b) = a `compare` b
  compare (EditRef a) (EditRef b) = a `compare` b
  compare (EditNoteRef a) (EditNoteRef b) = a `compare` b
  compare (EditorRef a) (EditorRef b) = a `compare` b
  compare (GenderRef a) (GenderRef b) = a `compare` b
  compare (LabelTypeRef a) (LabelTypeRef b) = a `compare` b
  compare (LanguageRef a) (LanguageRef b) = a `compare` b
  compare (RecordingRef a) (RecordingRef b) = a `compare` b
  compare (RelationshipTypeRef a) (RelationshipTypeRef b) = a `compare` b
  compare (ReleaseRef a) (ReleaseRef b) = a `compare` b
  compare (ReleaseGroupRef a) (ReleaseGroupRef b) = a `compare` b
  compare (ReleaseGroupTypeRef a) (ReleaseGroupTypeRef b) = a `compare` b
  compare (ReleasePackagingRef a) (ReleasePackagingRef b) = a `compare` b
  compare (ReleaseStatusRef a) (ReleaseStatusRef b) = a `compare` b
  compare (ScriptRef a) (ScriptRef b) = a `compare` b
  compare (RevisionRef a) (RevisionRef b) = a `compare` b
  compare (TreeRef a) (TreeRef b) = a `compare` b
  compare _ _ = error "Impossible condition: comparing references of different types"


--------------------------------------------------------------------------------
data Alias = Alias
    { aliasName :: Text
    , aliasSortName :: Text
    , aliasBeginDate :: PartialDate
    , aliasEndDate :: PartialDate
    , aliasEnded :: Bool
    , aliasType :: Maybe (Ref AliasType)
    , aliasLocale :: Maybe Text
    }
  deriving (Eq, Ord, Show)


data AliasType = AliasType
    { aliasTypeName :: Text }
  deriving (Eq, Show)


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
the actual artists name.

This type is completely uninhabited, and you should work with artist credits
through lists of 'ArtistCreditName'. -}
data ArtistCredit


--------------------------------------------------------------------------------
{-| An individual artist credit in an 'ArtistCredit'. This can also be thought
of as a single 'Artist' appearing in an 'ArtistCredit'. -}
data ArtistCreditName = ArtistCreditName
    { acnArtist :: Ref Artist
    , acnName :: Text
    , acnJoinPhrase :: Text
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| The definition of a type of an artist (e.g., \"person\" or \"group\") . -}
data ArtistType = ArtistType
    { artistTypeName :: Text }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| A country where artists resides, labels are founded, releases are released
in, etc. -}
data Country = Country
    { countryIsoCode :: Text
      -- ^ The ISO-3166 code for this country
    , countryName :: Text
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| A MusicBrainz editor who makes changes to the database. -}
data Editor = Editor { editorName :: Text }
  deriving (Eq, Show)


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


--------------------------------------------------------------------------------
data IPI = IPI
    { ipiCode :: Text }
  deriving (Eq, Ord, Show)


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


--------------------------------------------------------------------------------
{-| A language that is written or spoken. -}
data Language = Language { languageName :: Text
                         , languageIsoCode2t :: Text
                         , languageIsoCode2b :: Text
                         , languageIsoCode1 :: Text
                         , languageIsoCode3  :: Text
                         }


--------------------------------------------------------------------------------
{-| A recording in MusicBrainz (which is realised on 'Tracklist' as a
'Track'. -}
data Recording = Recording
    { recordingName :: Text
    , recordingComment :: Text
    , recordingArtistCredit :: Ref ArtistCredit
    , recordingDuration :: Int
    }
  deriving (Eq, Show, Typeable)


--------------------------------------------------------------------------------
{-| A release in MusicBrainz is a physical product that people can purchase,
and belongs to a 'ReleaseGroup' and consists of some information along with
multiple 'Medium's. -}
data Release = Release
    { releaseName :: Text
    , releaseComment :: Text
    , releaseArtistCredit :: Ref ArtistCredit
    , releaseReleaseGroup :: Ref ReleaseGroup
    , releaseDate :: PartialDate
    , releaseCountry :: Maybe (Ref Country)
    , releaseScript :: Maybe (Ref Script)
    , releaseLanguage :: Maybe (Ref Language)
    , releasePackaging :: Maybe (Ref ReleasePackaging)
    , releaseStatus :: Maybe (Ref ReleaseStatus)
    }
  deriving (Eq, Show, Typeable)


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


--------------------------------------------------------------------------------
{-| A type index for 'ReleaseGroupType' indicating that this 'ReleaseGroupType'
is primary and can only occur once. -}
data Primary

{-| A release group type indicates the various types a release group can be.
For example, one release group type combination might be 'Album + Remix' to
indicate a remix album.

The parameter to 'ReleaseGroupType' indicates whether the release group
type is primary or secondary. -}
data ReleaseGroupType a = ReleaseGroupType
    { releaseGroupTypeName :: Text }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| The type of packaging a release came in. -}
data ReleasePackaging = ReleasePackaging { releasePackagingName :: Text }


--------------------------------------------------------------------------------
{-| A release status indicates whether a 'Release' was released official,
promotionally, as a bootleg, etc. -}
data ReleaseStatus = ReleaseStatus { releaseStatusName :: Text }


--------------------------------------------------------------------------------
{-| The script that text is written. -}
data Script = Script { scriptIsoCode :: Text
                     , scriptIsoNumber :: Text
                     , scriptName :: Text
                     }


--------------------------------------------------------------------------------
{-| A partial date consisting of an optional year, month and day.-}
data PartialDate = PartialDate
    { dateYear :: Maybe Int
    , dateMonth :: Maybe Int
    , dateDay :: Maybe Int
    }
  deriving (Eq, Ord, Show)


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
  deriving (Eq, Ord, Show, Typeable)


{-| Inject a 'String' into an 'MBID', or extract a 'String' from an 'MBID'. To
work with this projection, you should use '^?' (to inject with a chance of
failure) and '^.'/'by' (to extract):

> "10adbe5e-a2c0-4bf3-8249-2b4cbf6e6ca8" ^? mbid :: Maybe (MBID a)

> aValidMbidValue ^. by mbid :: String
-}
mbid :: SimpleProjection String (MBID a)
mbid = projection mbidToString parseMbid
  where
    parseMbid = fmap MBID . fromString
    mbidToString (MBID m) = toString m


--------------------------------------------------------------------------------
{-| Represents a view of a versioned MusicBrainz \'core\' entity at a specific
point in time (a specific 'Revision'). -}
data CoreEntity a = CoreEntity
    { coreRef :: Ref a
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


--------------------------------------------------------------------------------
{-| Trees for entities are a somewhat internal concept of the way MusicBrainz
versioning works. A tree consists of all the data that is versioned for a
specific entity (of type @a@). -}
data Tree a where
  ArtistTree :: {
    artistData :: Artist
  , artistRelationships :: Set.Set LinkedRelationship
  , artistAliases :: Set.Set Alias
  , artistIpiCodes :: Set.Set IPI
  , artistAnnotation :: Text
  } -> Tree Artist

  LabelTree :: {
    labelData :: Label
  } -> Tree Label

  RecordingTree :: {
    recordingData :: Recording
  } -> Tree Recording

  ReleaseTree :: {
    releaseData :: Release
  } -> Tree Release

  ReleaseGroupTree :: {
    releaseGroupData :: ReleaseGroup
  } -> Tree ReleaseGroup

deriving instance Eq (Tree a)
deriving instance Show (Tree a)

treeData :: Tree a -> a
treeData ArtistTree{..} = artistData
treeData LabelTree{..} = labelData
treeData RecordingTree{..} = recordingData
treeData ReleaseTree{..} = releaseData
treeData ReleaseGroupTree{..} = releaseGroupData

--------------------------------------------------------------------------------
{-| An edit bundles up multiple 'Revision's that have not yet been applied to
entities. Editors can then vote on these edits to decide if they should be
merge, which ModBot can then later merge (or reject) once a consensus
emerges. -}
data Edit = Edit
    { editStatus :: EditStatus
    }


--------------------------------------------------------------------------------
{-| The possible states an edit can be in. -}
data EditStatus = Open | Closed
  deriving (Eq, Show)

instance Enum EditStatus where
  fromEnum Open = 1
  fromEnum Closed = 2

  toEnum 1 = Open
  toEnum 2 = Closed
  toEnum n = error $ show n ++ " cannot be converted to EditStatus"

  enumFrom = boundedEnumFrom

instance Bounded EditStatus where
  minBound = Open
  maxBound = Closed


--------------------------------------------------------------------------------
{-| The possible types of votes that editors can cast on an edit. -}
data Vote = Accept | Reject | Abstain
  deriving (Eq, Show)

-- A custom instance here allows us to use -1 for reject.
instance Enum Vote where
  fromEnum Accept = 1
  fromEnum Reject = -1
  fromEnum Abstain = 0

  toEnum 1 = Accept
  toEnum (-1) = Reject
  toEnum 0 = Abstain
  toEnum n = error $ show n ++ " cannot be converted to Vote"

  enumFrom = boundedEnumFrom

instance Bounded Vote where
  minBound = Reject
  maxBound = Accept

--------------------------------------------------------------------------------
{-| An edit note is a comment that can be left by editors on edit notes, to
have a discussion about the changes being made, or to provide references for
other editors to verify changes against. -}
data EditNote = EditNote
    { editNoteBody :: Text
    , editNoteAuthor :: Ref Editor
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data LinkedRelationship =
  ArtistRelationship
    { relTarget :: Ref Artist
    , relData :: Relationship
    }
  deriving (Eq, Ord, Show)


data Relationship = Relationship
    { relType :: Ref RelationshipType
    , relAttributes :: Set.Set (Ref RelationshipAttribute)
    , relBeginDate :: PartialDate
    , relEndDate :: PartialDate
    , relEnded :: Bool
    }
  deriving (Eq, Ord, Show)


data RelationshipType = RelationshipType
    { relName :: Text }
  deriving (Eq, Ord, Show)


data RelationshipAttribute = RelationshipAttribute
    { relAttributeName :: Text }
  deriving (Eq, Ord, Show)
