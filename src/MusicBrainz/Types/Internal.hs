{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-| Definitions of all types used within MusicBrainz. This exports *almost*
everything, but not actually everything. For example, we definitely want to
always go through 'ipi' and 'iswc' to construct 'ISWC's.-}
module MusicBrainz.Types.Internal
    (
      Artist(..)
    , Alias(..)
    , AliasType(..)
    , ArtistCredit
    , ArtistCreditName(..)
    , ArtistType(..)
    , CdToc(..)
    , CoreEntity(..)
    , Country(..)
    , Edit(..)
    , EditNote(..)
    , EditStatus(..)
    , Editor(..)
    , Entity(..)
    , Gender(..)
    , IPI, ipi
    , ISRC, isrc
    , ISWC, iswc
    , Label(..)
    , LabelType(..)
    , Language(..)
    , LinkedRelationship(..)
    , MBID(..), mbid
    , Medium(..)
    , MediumFormat(..)
    , PUID(..), puid
    , PartialDate(..)
    , Recording(..)
    , Ref
    , RefSpec
    , Referenceable
    , Relationship(..)
    , RelationshipAttribute(..)
    , RelationshipTarget(..)
    , RelationshipType(..)
    , Release(..)
    , ReleaseGroup(..)
    , ReleaseGroupType(..), Primary, Secondary
    , ReleaseLabel(..)
    , ReleasePackaging(..)
    , ReleaseStatus(..)
    , Revision
    , Script(..)
    , Track(..)
    , Tree(..)
    , Url(..)
    , Vote(..)
    , VoteScore(..)
    , Work(..)
    , WorkType(..)
    , reference, dereference
    , emptyDate, isEmpty
    , treeData
    ) where

import Prelude hiding ((.))

import Control.Applicative hiding (optional)
import Control.Category ((.))
import Control.Lens
import Control.Monad (mfilter)
import Data.Functor.Identity (Identity)
import Data.Monoid (mconcat, (<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID
import GHC.Enum (boundedEnumFrom)
import Network.URI (URI)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text ()

import qualified Data.Set as Set
import qualified Data.Text as T

--------------------------------------------------------------------------------
{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data Ref a = Referenceable a => Ref (RefSpec a)

deriving instance Eq (Ref a)
deriving instance Ord (Ref a)
deriving instance Show (Ref a)

{-| The family of types which can be referenced via a primary key. -}
class (Eq (RefSpec a), Ord (RefSpec a), Show (RefSpec a)) => Referenceable a where
  {-| The exact type of all attributes that make up a reference. For example,
  a PostgreSQL @SERIAL@ field would be 'Int', while a compound key might be
  @(@'Int'@, @'Int'). -}
  type RefSpec a :: *

instance Referenceable AliasType where
  type RefSpec AliasType = Int

instance Referenceable Artist where
  type RefSpec Artist = MBID Artist

instance Referenceable ArtistCredit where
  type RefSpec ArtistCredit = Int

instance Referenceable ArtistType where
  type RefSpec ArtistType = Int

instance Referenceable Country where
  type RefSpec Country = Int

instance Referenceable Edit where
  type RefSpec Edit = Int

instance Referenceable Editor where
  type RefSpec Editor = Int

instance Referenceable EditNote where
  type RefSpec EditNote = Int

instance Referenceable Gender where
  type RefSpec Gender = Int

instance Referenceable Label where
  type RefSpec Label = MBID Label

instance Referenceable LabelType where
  type RefSpec LabelType = Int

instance Referenceable Language where
  type RefSpec Language = Int

instance Referenceable MediumFormat where
  type RefSpec MediumFormat = Int

instance Referenceable Recording where
  type RefSpec Recording = MBID Recording

instance Referenceable RelationshipAttribute where
  type RefSpec RelationshipAttribute = Int

instance Referenceable RelationshipType where
  type RefSpec RelationshipType = Int

instance Referenceable Release where
  type RefSpec Release = MBID Release

instance Referenceable ReleaseGroup where
  type RefSpec ReleaseGroup = MBID ReleaseGroup

instance Referenceable (ReleaseGroupType a) where
  type RefSpec (ReleaseGroupType a) = Int

instance Referenceable ReleasePackaging where
  type RefSpec ReleasePackaging = Int

instance Referenceable ReleaseStatus where
  type RefSpec ReleaseStatus = Int

instance Referenceable (Revision a) where
  type RefSpec (Revision a) = Int

instance Referenceable Script where
  type RefSpec Script = Int

instance Referenceable (Tree a) where
  type RefSpec (Tree a) = Int

instance Referenceable Url where
  type RefSpec Url = MBID Url

instance Referenceable Work where
  type RefSpec Work = MBID Work

instance Referenceable WorkType where
  type RefSpec WorkType = Int

--------------------------------------------------------------------------------
{-| Unpack a reference into its individual attributes. -}
dereference :: Referenceable a => Ref a -> RefSpec a
dereference  = view (from reference)


--------------------------------------------------------------------------------
{-| An 'Iso'morphism to move between a set of attributes and a reference, and
back again. -}
reference :: Referenceable a => Simple Iso (RefSpec a) (Ref a)
reference = iso Ref (\(Ref r) -> r)


--------------------------------------------------------------------------------
{-| An alias is an alternative name for an entity, along with some information
describing what that name represents, which locale it is for, and when it was
in use. -}
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


--------------------------------------------------------------------------------
{-| A description of the type of an alias. -}
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
  deriving (Eq, Show)


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
data Editor = Editor { editorName :: Text
                     , editorPassword :: Text
                     }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| An 'Entity' is something that has been loaded from the database. It cotains
both data about itself (in @entityData@), and also a reference to itself (in
@entityRef@) so that other data/entities can refer to it. -}
data Entity a = Entity { entityRef :: Ref a
                       , entityData :: a
                       }

deriving instance (Eq a, Show a) => Eq (Entity a)
deriving instance (Eq a, Show a) => Show (Entity a)

--------------------------------------------------------------------------------
{-| The gender of an artist or editor. -}
data Gender = Gender
    { genderName :: Text }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| An \'Interested Parties Information Code\' that can be attached to various
entities. -}
newtype IPI = IPI Text
  deriving (Eq, Ord, Show, Typeable)

ipi :: SimplePrism Text IPI
ipi = parsecPrism (\(IPI i) -> i) ipiParser
  where
    ipiParser = IPI . T.pack <$> (try parseIpi <|> parseCae)
      where
        parseCae = ("00" ++) <$> count 9 digit <* eof
        parseIpi = count 11 digit <* eof


--------------------------------------------------------------------------------
{-| A label who is repsonsible for releasing/distributing music. -}
data Label = Label { labelName :: Text
                   , labelSortName :: Text
                   , labelComment :: Text
                   , labelBeginDate :: PartialDate
                   , labelEndDate :: PartialDate
                   , labelEnded :: Bool
                   , labelType :: Maybe (Ref LabelType)
                   , labelCode :: Maybe Int
                   , labelCountry :: Maybe (Ref Country)
                   }
  deriving (Eq, Show)


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
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| A recording in MusicBrainz (which is realised on 'Tracklist' as a
'Track'. -}
data Recording = Recording
    { recordingName :: Text
    , recordingComment :: Text
    , recordingArtistCredit :: Ref ArtistCredit
    , recordingDuration :: Maybe Int
    }
  deriving (Eq, Show)


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
  deriving (Eq, Show)


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
    , releaseGroupSecondaryTypes :: Set.Set (Ref (ReleaseGroupType Secondary))
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
{-| A type index for 'ReleaseGroupType' indicating that this 'ReleaseGroupType'
is primary and can only occur once. -}
data Primary

{-| A type index for 'ReleaseGroupType' indicating that this 'ReleaseGroupType'
is secondary and can only occur multiple times. -}
data Secondary

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
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data Work = Work
    { workName :: Text
    , workComment :: Text
    , workType :: Maybe (Ref WorkType)
    , workLanguage :: Maybe (Ref Language)
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data WorkType = WorkType { workTypeName :: Text }
  deriving (Eq, Show)


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
  deriving (Eq, Ord, Read, Show)

instance Wrapped UUID UUID (MBID a) (MBID a) where
  wrapped = iso MBID $ \(MBID a) -> a

{-| Inject a 'String' into an 'MBID', or extract a 'String' from an 'MBID'. To
work with this 'Prism', you should use '^?' to convert strings to MBIDs (there
is a chance of failure) and '^.' / 'remit' to extract the 'String' from an
'MBID':

> "10adbe5e-a2c0-4bf3-8249-2b4cbf6e6ca8" ^? mbid :: Maybe (MBID a)

> aValidMbidValue ^. remit mbid :: String
-}
mbid :: SimplePrism String (MBID a)
mbid = uuid.wrapped


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
  , labelRelationships :: Set.Set LinkedRelationship
  , labelAliases :: Set.Set Alias
  , labelIpiCodes :: Set.Set IPI
  , labelAnnotation :: Text
  } -> Tree Label

  RecordingTree :: {
    recordingData :: Recording
  , recordingRelationships :: Set.Set LinkedRelationship
  , recordingAnnotation :: Text
  , recordingIsrcs :: Set.Set ISRC
  , recordingPuids :: Set.Set PUID
  } -> Tree Recording

  ReleaseTree :: {
    releaseData :: Release
  , releaseRelationships :: Set.Set LinkedRelationship
  , releaseAnnotation :: Text
  , releaseLabels :: Set.Set ReleaseLabel
  , releaseMediums :: [Medium]
  } -> Tree Release

  ReleaseGroupTree :: {
    releaseGroupData :: ReleaseGroup
  , releaseGroupRelationships :: Set.Set LinkedRelationship
  , releaseGroupAnnotation :: Text
  } -> Tree ReleaseGroup

  UrlTree :: {
    urlData :: Url
  , urlRelationships :: Set.Set LinkedRelationship
  } -> Tree Url

  WorkTree :: {
    workData :: Work
  , workRelationships :: Set.Set LinkedRelationship
  , workAliases :: Set.Set Alias
  , workAnnotation :: Text
  , workIswcs :: Set.Set ISWC
  } -> Tree Work

deriving instance Eq (Tree a)
deriving instance Show (Tree a)

{-| A convenience accessor to the \'essential\' data inside a tree (the data
which contains the entities, and so on.) -}
treeData :: Tree a -> a
treeData ArtistTree{..} = artistData
treeData LabelTree{..} = labelData
treeData RecordingTree{..} = recordingData
treeData ReleaseTree{..} = releaseData
treeData ReleaseGroupTree{..} = releaseGroupData
treeData UrlTree{..} = urlData
treeData WorkTree{..} = workData

--------------------------------------------------------------------------------
{-| An edit bundles up multiple 'Revision's that have not yet been applied to
entities. Editors can then vote on these edits to decide if they should be
merge, which ModBot can then later merge (or reject) once a consensus
emerges. -}
data Edit = Edit
    { editStatus :: EditStatus
    }
  deriving (Eq, Show)


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
data VoteScore = Accept | Reject | Abstain
  deriving (Eq, Show)

-- A custom instance here allows us to use -1 for reject.
instance Enum VoteScore where
  fromEnum Accept = 1
  fromEnum Reject = -1
  fromEnum Abstain = 0

  toEnum 1 = Accept
  toEnum (-1) = Reject
  toEnum 0 = Abstain
  toEnum n = error $ show n ++ " cannot be converted to Vote"

  enumFrom = boundedEnumFrom

instance Bounded VoteScore where
  minBound = Reject
  maxBound = Accept


--------------------------------------------------------------------------------
{-| A vote on an edit. -}
data Vote = Vote { voteVote :: VoteScore
                 , voteEditor :: Ref Editor
                 , voteSuperceded :: Bool
                 }
  deriving (Eq, Show)


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
{-| A 'LinkedRelationship' is an end-point 'Ref', along with the data
describing the 'Relationship' itself. -}
data LinkedRelationship
  = ArtistRelationship (Ref Artist) Relationship
  | LabelRelationship (Ref Label) Relationship
  | RecordingRelationship (Ref Recording) Relationship
  | ReleaseRelationship (Ref Release) Relationship
  | ReleaseGroupRelationship (Ref ReleaseGroup) Relationship
  | UrlRelationship (Ref Url) Relationship
  | WorkRelationship (Ref Work) Relationship
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
{-| Metadata about a relationship between 2 entities. -}
data Relationship = Relationship
    { relType :: Ref RelationshipType
    , relAttributes :: Set.Set (Ref RelationshipAttribute)
    , relBeginDate :: PartialDate
    , relEndDate :: PartialDate
    , relEnded :: Bool
    }
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
{-| The type of a relationship between 2 entities. -}
data RelationshipType = RelationshipType
    { relName :: Text }
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
{-| An attribute of a relationship (for example, indicating whether the
relationship is additional). -}
data RelationshipAttribute = RelationshipAttribute
    { relAttributeName :: Text }
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
{-| A description of all types that a relationship can be between. -}
data RelationshipTarget = ToArtist | ToLabel | ToRecording | ToRelease | ToReleaseGroup | ToUrl | ToWork
  deriving (Bounded, Enum)


--------------------------------------------------------------------------------
newtype ISWC = ISWC Text
  deriving (Eq, Ord, Show, Typeable)

iswc :: SimplePrism Text ISWC
iswc = parsecPrism (\(ISWC t) -> t) iswcParser
  where
    iswcParser = do
      char 'T' *> dash
      digits <- sequence [ count 3 digit <* dot
                         , count 3 digit <* dot
                         , count 3 digit <* oneOf ".-"
                         ]
      fin <- digit <* eof
      pure $ ISWC $ formatIswc digits fin
      where
        optChar = optional . char
        dash = optChar '-'
        dot  = optChar '.'
        formatIswc digits fin =
          let digits' = T.intercalate "." (map T.pack digits)
          in "T-" <> digits' <> "-" <> T.pack [fin]


--------------------------------------------------------------------------------
newtype ISRC = ISRC Text
  deriving (Eq, Ord, Show, Typeable)

isrc :: SimplePrism Text ISRC
isrc = parsecPrism (\(ISRC i) -> i) isrcParser
  where
    isrcParser = do
      ISRC . mconcat . map T.pack <$>
        sequence [ countryCode, count 3 upperNum, count 7 digit ] <* eof
      where
        countryCode = mfilter (/= "TC") $ count 2 upper
        upperNum = upper <|> digit


--------------------------------------------------------------------------------
data Url = Url { urlUrl :: URI }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data ReleaseLabel = ReleaseLabel
    { releaseLabel :: Maybe (Ref Label)
    , releaseCatalogNumber :: Maybe Text
    }
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Medium = Medium
    { mediumName :: Text
    , mediumFormat :: Maybe (Ref MediumFormat)
    , mediumPosition :: Int
    , mediumTracks :: [Track]
    , mediumCdTocs :: Set.Set CdToc
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data MediumFormat = MediumFormat { mediumFormatName :: Text }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
data Track = Track
    { trackName :: Text
    , trackRecording :: Ref Recording
    , trackDuration :: Maybe Int
    , trackArtistCredit :: Ref ArtistCredit
    , trackPosition :: Text
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
newtype PUID = PUID UUID
  deriving (Eq, Ord, Show)

instance Wrapped UUID UUID PUID PUID where
  wrapped = iso PUID $ \(PUID p) -> p

puid :: SimplePrism String PUID
puid = uuid.wrapped


--------------------------------------------------------------------------------
uuid :: SimplePrism String UUID
uuid = prism toString parseUUID
  where parseUUID s = case fromString s of
          Just u -> Right u
          Nothing -> Left s


--------------------------------------------------------------------------------
parsecPrism :: Stream a Identity Char => (c -> a) -> Parsec a () c -> SimplePrism a c
parsecPrism extract parser = prism extract runParse
  where
    runParse t = either (const $ Left t) Right $
      parse parser "" t


--------------------------------------------------------------------------------
data CdToc = CdToc
    { cdTocTrackOffsets :: [Int]
    , cdTocLeadoutOffset :: Int
    }
  deriving (Eq, Ord, Show)

