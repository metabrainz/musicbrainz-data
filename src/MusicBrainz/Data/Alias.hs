{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Functions for working with entity aliases. -}
module MusicBrainz.Data.Alias
    ( ViewAliases(..)
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import MusicBrainz
import MusicBrainz.Data.CoreEntity

import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| This type class provides functions for working with aliases for specific
entity types. -}
class ViewAliases a where
  {-| Fetch all aliases for a given revision of an entity. -}
  viewAliases :: (Functor m, MonadIO m) => Ref (Revision a) -> MusicBrainzT m (Set.Set (Alias a))

  default viewAliases
    :: (Functor m, MonadIO m, CoreEntityTable a)
    => Ref (Revision a) -> MusicBrainzT m (Set.Set (Alias a))
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
