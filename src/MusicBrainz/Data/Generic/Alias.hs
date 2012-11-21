{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Generic.Alias where

import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Database.PostgreSQL.Simple (Only(..))

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
viewAliases :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m (Set.Set Alias)
viewAliases entityName r = Set.fromList <$> query q (Only r)
  where
    q = fromString $ unlines
          [ "SELECT name.name, sort_name.name,"
          , "begin_date_year, begin_date_month, begin_date_day,"
          , "end_date_year, end_date_month, end_date_day,"
          , "ended, " ++ entityName ++ "_alias_type_id, locale "
          , "FROM " ++ entityName ++ "_alias alias"
          , "JOIN " ++ entityName ++ "_name name ON (alias.name = name.id) "
          , "JOIN " ++ entityName ++ "_name sort_name ON (alias.sort_name = sort_name.id) "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]
