{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ReleaseGroupType's. -}
module MusicBrainz.Data.ReleaseGroupType ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance ResolveReference (ReleaseGroupType Primary) where
  resolveReference rgTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM release_group_primary_type WHERE id = ? |]
      (Only rgTypeId)
