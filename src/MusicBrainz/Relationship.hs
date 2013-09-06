{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Relationship where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple ((:.)(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Monad
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Entity
import MusicBrainz.PartialDate (PartialDate)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)

import {-# SOURCE #-} MusicBrainz.Artist (Artist)
import {-# SOURCE #-} MusicBrainz.Label (Label)
import {-# SOURCE #-} MusicBrainz.Recording (Recording)
import {-# SOURCE #-} MusicBrainz.Release (Release)
import {-# SOURCE #-} MusicBrainz.ReleaseGroup (ReleaseGroup)
import {-# SOURCE #-} MusicBrainz.URL (URL)
import {-# SOURCE #-} MusicBrainz.Work (Work)

import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| A description of all types that a relationship can be between. -}
data RelationshipTarget = ToArtist | ToLabel | ToRecording | ToRelease | ToReleaseGroup | ToURL | ToWork
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

instance FromField RelationshipTarget where
  fromField f Nothing =
    FF.returnError FF.UnexpectedNull f "RelationshipTarget cannot be null"

  fromField f (Just v) = case v of
    "artist" -> return ToArtist
    "label" -> return ToLabel
    "recording" -> return ToRecording
    "release" -> return ToRelease
    "release_group" -> return ToReleaseGroup
    "url" -> return ToURL
    "work" -> return ToWork
    _ -> FF.returnError FF.ConversionFailed f "Unknown relationship target"

instance ToField RelationshipTarget where
  toField v = toField $ flip asTypeOf ([]::String) $ case v of
    ToArtist -> "artist"
    ToLabel -> "label"
    ToRecording -> "recording"
    ToRelease -> "release"
    ToReleaseGroup -> "release_group"
    ToURL -> "url"
    ToWork -> "work"


--------------------------------------------------------------------------------
{-| An attribute of a relationship (for example, indicating whether the
relationship is additional). -}
data RelationshipAttribute = RelationshipAttribute
    { relAttributeName :: !Text
    , relAttributeParent :: !(Maybe (Ref RelationshipAttribute))
    , relAttributeRoot :: !(Ref RelationshipAttribute)
    , relAttributeChildOrder :: !Int
    , relAttributeDescription :: !Text
    }
  deriving (Eq, Ord, Show)

instance Referenceable RelationshipAttribute where
  type RefSpec RelationshipAttribute = Int

instance FromField (Ref RelationshipAttribute) where
  fromField f v = view reference <$> fromField f v

instance FromRow RelationshipAttribute where
  fromRow = RelationshipAttribute <$> field <*> field <*> field <*> field <*> field

instance ToField (Ref RelationshipAttribute) where
  toField = toField . dereference

instance ToRow RelationshipAttribute where
  toRow RelationshipAttribute{..} = [ toField relAttributeRoot
                                    , toField relAttributeParent
                                    , toField relAttributeChildOrder
                                    , toField relAttributeName
                                    , toField relAttributeDescription
                                    ]

instance ResolveReference RelationshipAttribute where
  resolveReference attributeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM link_attribute_type WHERE id = ? |]
      (Only attributeId)


--------------------------------------------------------------------------------
addRelationshipAttributeType :: (Functor m, MonadIO m)
  => Text
  -> Maybe (Ref RelationshipAttribute)
  -> Maybe (Ref RelationshipAttribute)
  -> Int
  -> Text
  -> MusicBrainzT m (Entity (RelationshipAttribute))
addRelationshipAttributeType name parent root childOrder description = do
    id' <- selectValue $ query_ [sql| SELECT nextval('link_attribute_type_id_seq') |]

    head <$>
      query [sql|
              INSERT INTO link_attribute_type
                (id, gid, parent, child_order, name, description, root)
              VALUES ( ?, uuid_generate_v4(), ?, ?, ?, ?, ?)
              RETURNING id, name, parent, root, child_order, description
            |] ( id' :: Int, parent, childOrder, name, description
               , fromMaybe (id' ^. reference) root
               )


--------------------------------------------------------------------------------
{-| A description of the usage of a 'RelationshipAttribute' by a
'RelationshipType'. -}
data RelationshipAttributeUse = RelationshipAttributeUse
     { relAttribute :: !(Ref RelationshipAttribute)
     , relAttributeMinOccurances :: !(Maybe Int)
     , relAttributeMaxOccurances :: !(Maybe Int)
     }
  deriving (Eq, Ord, Show)

instance FromRow RelationshipAttributeUse where
  fromRow = RelationshipAttributeUse <$> field <*> field <*> field

instance ToRow RelationshipAttributeUse where
  toRow RelationshipAttributeUse{..} =
    [ toField relAttribute
    , toField relAttributeMinOccurances
    , toField relAttributeMaxOccurances
    ]


--------------------------------------------------------------------------------
{-| The type of a relationship between 2 entities. -}
data RelationshipType = RelationshipType
    { relName :: !Text
    , relTypeAttributes :: !(Set RelationshipAttributeUse)
    , relParent :: !(Maybe (Ref RelationshipType))
    , relChildOrder :: !Int
    , relLeftTarget :: !RelationshipTarget
    , relRightTarget :: !RelationshipTarget
    , relDescription :: !Text
    , relLinkPhrase :: !Text
    , relReverseLinkPhrase :: !Text
    , relShortLinkPhrase :: !Text
    , relPriority :: !Int
    }
  deriving (Eq, Ord, Show)

instance Referenceable RelationshipType where
  type RefSpec RelationshipType = Int

instance FromField (Ref RelationshipType) where
  fromField f v = view reference <$> fromField f v

instance ToField (Ref RelationshipType) where
  toField = toField . dereference

instance ToRow RelationshipType where
  toRow RelationshipType{..} = [ toField relName
                               , toField relParent
                               , toField relChildOrder
                               , toField relLeftTarget
                               , toField relRightTarget
                               , toField relDescription
                               , toField relLinkPhrase
                               , toField relReverseLinkPhrase
                               , toField relShortLinkPhrase
                               , toField relPriority
                               ]

instance Add RelationshipType where
  add rt = do
    ((id', name, parent, childOrder, t0, t1, description) :. (linkPhrase, reverseLinkPhrase, shortLinkPhrase, priority)) <-
      head <$> query
        [sql|
          INSERT INTO link_type (name, parent, child_order, entity_type0,
            entity_type1, description, link_phrase, reverse_link_phrase,
            short_link_phrase, priority, gid)
          VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, uuid_generate_v4() )
          RETURNING id, name, parent, child_order, entity_type0, entity_type1,
            description, link_phrase, reverse_link_phrase, short_link_phrase,
            priority
        |] rt

    attrs <- returning
      [sql|
        INSERT INTO link_type_attribute_type
          (link_type, attribute_type, min, max)
        VALUES (?, ?, ?, ?)
        RETURNING attribute_type, min, max
      |] (map ((Only id') :.) $ Set.toList $ relTypeAttributes rt)

    return Entity
      { entityRef = id'
      , entityData =
          RelationshipType
            { relName = name
            , relTypeAttributes = Set.fromList attrs
            , relParent = parent
            , relChildOrder = childOrder
            , relLeftTarget = t0
            , relRightTarget = t1
            , relDescription = description
            , relLinkPhrase = linkPhrase
            , relReverseLinkPhrase = reverseLinkPhrase
            , relShortLinkPhrase = shortLinkPhrase
            , relPriority = priority
            }
      }

instance ResolveReference RelationshipType where
  resolveReference relationshipTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM link_type WHERE id = ? |]
      (Only relationshipTypeId)


--------------------------------------------------------------------------------
{-| Metadata about a relationship between 2 entities. -}
data Relationship = Relationship
    { relType :: !(Ref RelationshipType)
    , relAttributes :: !(Set (Ref RelationshipAttribute))
    , relBeginDate :: !PartialDate
    , relEndDate :: !PartialDate
    , relEnded :: !Bool
    }
  deriving (Eq, Ord, Show)

instance ToRow Relationship where
  toRow Relationship{..} = [ toField relType ]
                           ++ toRow relBeginDate
                           ++ toRow relEndDate
                           ++
                           [ toField relEnded ]


--------------------------------------------------------------------------------
{-| A 'LinkedRelationship' is an end-point 'Ref', along with the data
describing the 'Relationship' itself. -}
data LinkedRelationship
  = ArtistRelationship !(Ref Artist) !Relationship
  | LabelRelationship !(Ref Label) !Relationship
  | RecordingRelationship !(Ref Recording) !Relationship
  | ReleaseRelationship !(Ref Release) !Relationship
  | ReleaseGroupRelationship !(Ref ReleaseGroup) !Relationship
  | URLRelationship !(Ref URL) !Relationship
  | WorkRelationship !(Ref Work) !Relationship
  deriving (Eq, Ord, Show)

