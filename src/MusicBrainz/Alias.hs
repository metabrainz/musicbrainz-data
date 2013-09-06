{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Alias where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Set (Set)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Entity
import MusicBrainz.Monad
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Class.RootTable
import MusicBrainz.PartialDate (PartialDate)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Revision (Revision)

import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| An alias is an alternative name for an entity, along with some information
describing what that name represents, which locale it is for, and when it was
in use.

@a@ is a phantom type describing what type of entity this alias belongs to.-}
data Alias a = Alias
    { aliasName :: !Text
    , aliasSortName :: !Text
    , aliasBeginDate :: !PartialDate
    , aliasEndDate :: !PartialDate
    , aliasEnded :: !Bool
    , aliasType :: !(Maybe (Ref (AliasType a)))
    , aliasLocale :: !(Maybe Text)
    , aliasPrimaryForLocale :: !Bool
    }
  deriving (Eq, Ord, Show)

instance FromRow (Alias a) where
  fromRow = Alias <$> field <*> field <*> fromRow <*> fromRow <*> field <*> field
                  <*> field <*> field

instance ToRow (Alias a) where
  toRow Alias{..} = [ toField aliasName
                    , toField aliasSortName
                    ]
                    ++ toRow aliasBeginDate
                    ++ toRow aliasEndDate
                    ++
                    [ toField aliasEnded
                    , toField aliasType
                    , toField aliasLocale
                    , toField aliasPrimaryForLocale
                    ]


--------------------------------------------------------------------------------
{-| A description of the type of an alias. @a@ is a phantom type, which should
be one of 'ArtistAlias', 'LabelAlias' or 'WorkAlias'. It is used to signify
exactly which type of alias this is (as each entity has its own distinct
set of possible alias types.) -}
newtype AliasType a = AliasType { aliasTypeName :: Text }
  deriving (Eq, Show)

instance Referenceable (AliasType a) where
  type RefSpec (AliasType a) = Int

instance FromField (Ref (AliasType a)) where
  fromField f v = view reference <$> fromField f v

instance FromRow (AliasType a) where
  fromRow = AliasType <$> field

instance ToField (Ref (AliasType a)) where
  toField = toField . dereference

instance ToRow (AliasType a) where
  toRow AliasType{..} = [ toField aliasTypeName
                        ]

instance RootTable a => Add (AliasType a) where
  add type' = head <$>
    query sql type'
   where
    sql = fromString $ unwords
      [ "INSERT INTO"
      , untag (rootTable :: Tagged a String) ++ "_alias_type"
      , "(name) VALUES (?) RETURNING id, name"
      ]

instance RootTable a => ResolveReference (AliasType a) where
  resolveReference aliasTypeId = listToMaybe . map fromOnly <$>
    query sql (Only aliasTypeId)
   where
    sql = fromString $ unwords
      [ "SELECT id FROM"
      , untag (rootTable :: Tagged a String) ++ "_alias_type"
      , "WHERE id = ?"
      ]


--------------------------------------------------------------------------------
{-| This type class provides functions for working with aliases for specific
entity types. -}
class ViewAliases a where
  {-| Fetch all aliases for a given revision of an entity. -}
  viewAliases :: (Functor m, MonadIO m) => Ref (Revision a) -> MusicBrainzT m (Set.Set (Alias a))

  default viewAliases
    :: (Functor m, MonadIO m, RootTable a)
    => Ref (Revision a) -> MusicBrainzT m (Set (Alias a))
  viewAliases r = Set.fromList <$> query q (Only r)
   where
     entityName = untag (rootTable :: Tagged a String)
     q = fromString $ unlines
           [ "SELECT name.name, sort_name.name,"
           , "begin_date_year, begin_date_month, begin_date_day,"
           , "end_date_year, end_date_month, end_date_day,"
           , "ended, " ++ entityName ++ "_alias_type_id, locale, primary_for_locale "
           , "FROM " ++ entityName ++ "_alias alias"
           , "JOIN " ++ entityName ++ "_name name ON (alias.name = name.id) "
           , "JOIN " ++ entityName ++ "_name sort_name ON (alias.sort_name = sort_name.id) "
           , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
           , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
           , "WHERE revision_id = ?"
           ]
