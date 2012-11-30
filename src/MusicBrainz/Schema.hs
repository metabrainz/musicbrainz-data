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
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Artist) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref ArtistCredit) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref ArtistType) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Country) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Edit) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref EditNote) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Editor) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Gender) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Label) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref LabelType) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Language) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Recording) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref RelationshipAttribute) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref RelationshipType) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Release) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref ReleaseGroup) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref (ReleaseGroupType a)) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref ReleasePackaging) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref ReleaseStatus) where
  fromField f v = Ref <$> fromField f v


instance FromField (Ref Script) where
  fromField f v = Ref <$> fromField f v


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
  fromField f v = Ref <$> fromField f v


instance FromField (Ref (Tree a)) where
  fromField f v = Ref <$> fromField f v


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


instance FromRow LabelType where
  fromRow = LabelType <$> field


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


instance FromRow (ReleaseGroupType a) where
  fromRow = ReleaseGroupType <$> field


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
  toField = toField . dereference


instance ToField (Ref Artist) where
  toField = toField . dereference


instance ToField (Ref ArtistCredit) where
  toField = toField . dereference


instance ToField (Ref ArtistType) where
  toField = toField . dereference


instance ToField (Ref Country) where
  toField = toField . dereference


instance ToField (Ref Edit) where
  toField = toField . dereference


instance ToField (Ref Editor) where
  toField = toField . dereference


instance ToField (Ref Gender) where
  toField = toField . dereference


instance ToField (Ref Label) where
  toField = toField . dereference


instance ToField (Ref LabelType) where
  toField = toField . dereference


instance ToField (Ref Language) where
  toField = toField . dereference


instance ToField (Ref Recording) where
  toField = toField . dereference


instance ToField (Ref RelationshipType) where
  toField = toField . dereference


instance ToField (Ref Release) where
  toField = toField . dereference


instance ToField (Ref ReleaseGroup) where
  toField = toField . dereference


instance ToField (Ref (ReleaseGroupType a)) where
  toField = toField . dereference


instance ToField (Ref ReleasePackaging) where
  toField = toField . dereference


instance ToField (Ref ReleaseStatus) where
  toField = toField . dereference


instance ToField (Ref (Revision a)) where
  toField = toField . dereference


instance ToField (Ref Script) where
  toField = toField . dereference


instance ToField (Ref (Tree a)) where
  toField = toField . dereference


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


instance ToRow LabelType where
  toRow LabelType{..} = [ toField labelTypeName
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


instance ToRow (ReleaseGroupType a) where
  toRow ReleaseGroupType{..} = [ toField releaseGroupTypeName
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
