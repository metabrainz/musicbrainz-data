{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Contains instances of 'FromField', 'FromRow', 'ToField' and 'ToRow' to
serialize to the MusicBrainz PostgreSQL database, and retrieve data from
it. This module doesn't really export much, but importing it will bring in
many type classes instances. 'MusicBrainz' itself re-exports this module,
so you should usually import that. -}
module MusicBrainz.Schema () where

import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative
import Control.Lens
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError, typename)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..), inQuotes)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Edit
import MusicBrainz.Types.Internal

import qualified Data.ByteString.Char8 as LBS
import qualified Data.UUID as UUID

--------------------------------------------------------------------------------
instance FromField (Ref AliasType) where
  fromField f v = AliasTypeRef <$> fromField f v


instance FromField (Ref Artist) where
  fromField f v = ArtistRef <$> fromField f v


instance FromField (Ref ArtistCredit) where
  fromField f v = ArtistCreditRef <$> fromField f v


instance FromField (Ref ArtistType) where
  fromField f v = ArtistTypeRef <$> fromField f v


instance FromField (Ref Country) where
  fromField f v = CountryRef <$> fromField f v


instance FromField (Ref Edit) where
  fromField f v = EditRef <$> fromField f v


instance FromField (Ref EditNote) where
  fromField f v = EditNoteRef <$> fromField f v


instance FromField (Ref Editor) where
  fromField f v = EditorRef <$> fromField f v


instance FromField (Ref Gender) where
  fromField f v = GenderRef <$> fromField f v


instance FromField (Ref Label) where
  fromField f v = LabelRef <$> fromField f v


instance FromField (Ref LabelType) where
  fromField f v = LabelTypeRef <$> fromField f v


instance FromField (Ref Language) where
  fromField f v = LanguageRef <$> fromField f v


instance FromField (Ref Recording) where
  fromField f v = RecordingRef <$> fromField f v


instance FromField (Ref RelationshipAttribute) where
  fromField f v = RelationshipAttributeRef <$> fromField f v


instance FromField (Ref RelationshipType) where
  fromField f v = RelationshipTypeRef <$> fromField f v


instance FromField (Ref Release) where
  fromField f v = ReleaseRef <$> fromField f v


instance FromField (Ref ReleaseGroup) where
  fromField f v = ReleaseGroupRef <$> fromField f v


instance FromField (Ref (ReleaseGroupType a)) where
  fromField f v = ReleaseGroupTypeRef <$> fromField f v


instance FromField (Ref ReleasePackaging) where
  fromField f v = ReleasePackagingRef <$> fromField f v


instance FromField (Ref ReleaseStatus) where
  fromField f v = ReleaseStatusRef <$> fromField f v


instance FromField (Ref Script) where
  fromField f v = ScriptRef <$> fromField f v


instance Typeable a => FromField (MBID a) where
  fromField f Nothing = returnError UnexpectedNull f "MBID cannot be null"
  fromField f (Just v) | typename f /= "uuid" = incompatible
                       | otherwise            = tryParse
    where
      incompatible = returnError Incompatible f "MBIDs must be PG type 'uuid'"
      tryParse = case UUID.fromString (LBS.unpack v) of
        Just uuid -> return $ MBID uuid
        Nothing -> returnError ConversionFailed f "Not a valid MBID"


instance FromField (Ref (Revision a)) where
  fromField f v = RevisionRef <$> fromField f v


instance FromField (Ref (Tree a)) where
  fromField f v = TreeRef <$> fromField f v


--------------------------------------------------------------------------------
instance (FromField (Ref a), FromRow a, Typeable a) => FromRow (CoreEntity a) where
  fromRow = CoreEntity     -- Core entity's MBID
                       <$> field
                           -- The revision reference
                       <*> field
                           -- Delegetate to the actual entity to parse its data.
                       <*> fromRow


instance FromRow Alias where
  fromRow = Alias <$> field <*> field <*> fromRow <*> fromRow <*> field <*> field
                  <*> field


instance FromRow Artist where
  fromRow = Artist <$> field <*> field <*> field <*> fromRow
                   <*> fromRow <*> field <*> field <*> field
                   <*> field


instance FromRow ArtistType where
  fromRow = ArtistType <$> field


instance FromRow Country where
  fromRow = Country <$> field <*> field


instance FromRow EditNote where
  fromRow = EditNote <$> field <*> field


instance FromRow Editor where
  fromRow = Editor <$> field


instance (FromField (Ref a), FromRow a) => FromRow (Entity a) where
  fromRow = Entity     -- Entity reference
                   <$> field
                       -- Delegetate to the actual entity to parse its data.
                   <*> fromRow


instance FromRow Gender where
  fromRow = Gender <$> field


instance FromRow IPI where
  fromRow = IPI <$> field


instance FromRow Label where
  fromRow = Label <$> field <*> field <*> field <*> fromRow <*> fromRow
                  <*> field <*> field <*> field


instance FromRow Language where
  fromRow = Language <$> field <*> field <*> field <*> field <*> field


instance FromRow PartialDate where
  fromRow = PartialDate <$> field <*> field <*> field


instance FromRow Recording where
  fromRow = Recording <$> field <*> field <*> field <*> field


instance FromRow RelationshipType where
  fromRow = RelationshipType <$> field


instance FromRow Release where
  fromRow = Release <$> field <*> field <*> field <*> field <*> fromRow
                    <*> field <*> field <*> field <*> field <*> field


instance FromRow ReleaseGroup where
  fromRow = ReleaseGroup <$> field <*> field <*> field <*> field


instance FromRow ReleasePackaging where
  fromRow = ReleasePackaging <$> field


instance FromRow ReleaseStatus where
  fromRow = ReleaseStatus <$> field


instance FromRow Script where
  fromRow = Script <$> field <*> field <*> field


--------------------------------------------------------------------------------
instance ToField EditStatus where
  toField = toField . fromEnum


instance ToField (MBID a) where
  toField id' = Plain $ inQuotes . fromString $ (id' ^. by mbid)


instance ToField (Ref AliasType) where
  toField (AliasTypeRef id') = toField id'


instance ToField (Ref Artist) where
  toField (ArtistRef id') = toField id'


instance ToField (Ref ArtistCredit) where
  toField (ArtistCreditRef id') = toField id'


instance ToField (Ref ArtistType) where
  toField (ArtistTypeRef id') = toField id'


instance ToField (Ref Country) where
  toField (CountryRef id') = toField id'


instance ToField (Ref Edit) where
  toField (EditRef id') = toField id'


instance ToField (Ref Editor) where
  toField (EditorRef id') = toField id'


instance ToField (Ref Gender) where
  toField (GenderRef id') = toField id'


instance ToField (Ref Label) where
  toField (LabelRef id') = toField id'


instance ToField (Ref LabelType) where
  toField (LabelTypeRef id') = toField id'


instance ToField (Ref Language) where
  toField (LanguageRef id') = toField id'


instance ToField (Ref Recording) where
  toField (RecordingRef id') = toField id'


instance ToField (Ref RelationshipType) where
  toField (RelationshipTypeRef id') = toField id'


instance ToField (Ref Release) where
  toField (ReleaseRef id') = toField id'


instance ToField (Ref ReleaseGroup) where
  toField (ReleaseGroupRef id') = toField id'


instance ToField (Ref (ReleaseGroupType a)) where
  toField (ReleaseGroupTypeRef id') = toField id'


instance ToField (Ref ReleasePackaging) where
  toField (ReleasePackagingRef id') = toField id'


instance ToField (Ref ReleaseStatus) where
  toField (ReleaseStatusRef id') = toField id'


instance ToField (Ref (Revision a)) where
  toField (RevisionRef id') = toField id'


instance ToField (Ref Script) where
  toField (ScriptRef id') = toField id'


instance ToField (Ref (Tree a)) where
  toField (TreeRef id') = toField id'


instance ToField Vote where
  toField = toField . fromEnum


--------------------------------------------------------------------------------
instance ToRow Alias where
  toRow Alias{..} = [ toField aliasName
                    , toField aliasSortName
                    ]
                    ++ toRow aliasBeginDate
                    ++ toRow aliasEndDate
                    ++
                    [ toField aliasEnded
                    , toField aliasType
                    , toField aliasLocale
                    ]


instance ToRow Artist where
  toRow Artist{..} = [ toField artistName
                     , toField artistSortName
                     , toField artistComment
                     ]
                     ++ toRow artistBeginDate
                     ++ toRow artistEndDate
                     ++
                     [
                       toField artistEnded
                     , toField artistGender
                     , toField artistType
                     , toField artistCountry
                     ]


instance ToRow ArtistType where
  toRow ArtistType{..} = [ toField artistTypeName
                         ]


instance ToRow Country where
  toRow Country{..} = [ toField countryIsoCode
                      , toField countryName
                      ]


instance ToRow EditNote where
  toRow EditNote{..} = [ toField editNoteAuthor
                       , toField editNoteBody
                       ]


instance ToRow Editor where
  toRow Editor{..} = [ toField editorName
                     ]


instance ToRow Gender where
  toRow Gender{..} = [ toField genderName
                     ]


instance ToRow IPI where
  toRow IPI{..} = [ toField ipiCode
                  ]


instance ToRow Label where
  toRow Label{..} = [ toField labelName
                    , toField labelSortName
                    , toField labelComment
                    ]
                    ++ toRow labelBeginDate
                    ++ toRow labelEndDate
                    ++
                    [
                      toField labelEnded
                    , toField labelType
                    , toField labelCode
                    ]


instance ToRow Language where
  toRow Language{..} = [ toField languageName
                       , toField languageIsoCode2t
                       , toField languageIsoCode2b
                       , toField languageIsoCode1
                       , toField languageIsoCode3
                       ]


instance ToRow RelationshipType where
  toRow RelationshipType{..} = [ toField relName ]


instance ToRow Release where
  toRow Release{..} = [ toField releaseName
                      , toField releaseComment
                      , toField releaseArtistCredit
                      ]
                      ++ toRow releaseDate
                      ++
                      [ toField releaseCountry
                      , toField releaseScript
                      , toField releaseLanguage
                      , toField releasePackaging
                      , toField releaseStatus
                      ]


instance ToRow Recording where
  toRow Recording{..} = [ toField recordingName
                        , toField recordingComment
                        , toField recordingArtistCredit
                        , toField recordingDuration
                        ]


instance ToRow Relationship where
  toRow Relationship{..} = [ toField relType ]
                           ++ toRow relBeginDate
                           ++ toRow relEndDate
                           ++
                           [ toField relEnded ]


instance ToRow ReleaseGroup where
  toRow ReleaseGroup{..} = [ toField releaseGroupName
                           , toField releaseGroupComment
                           , toField releaseGroupArtistCredit
                           , toField releaseGroupPrimaryType
                           ]


instance ToRow ReleasePackaging where
  toRow ReleasePackaging{..} = [ toField releasePackagingName
                               ]


instance ToRow ReleaseStatus where
  toRow ReleaseStatus{..} = [ toField releaseStatusName
                            ]


instance ToRow Script where
  toRow Script{..} = [ toField scriptIsoCode
                     , toField scriptIsoNumber
                     , toField scriptName
                     ]


instance ToRow PartialDate where
  toRow (PartialDate y m d) = map toField [y, m, d]
