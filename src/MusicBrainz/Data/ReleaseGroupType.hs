{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions to work with 'ReleaseGroupType's. -}
module MusicBrainz.Data.ReleaseGroupType ( ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Only(..))

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add (ReleaseGroupType Primary) where
  add = addRgType "release_group_primary_type"


--------------------------------------------------------------------------------
instance ResolveReference (ReleaseGroupType Primary) where
  resolveReference = resolveRgType "release_group_primary_type"


--------------------------------------------------------------------------------
instance Add (ReleaseGroupType Secondary) where
  add = addRgType "release_group_secondary_type"


--------------------------------------------------------------------------------
instance ResolveReference (ReleaseGroupType Secondary) where
  resolveReference = resolveRgType "release_group_secondary_type"


--------------------------------------------------------------------------------
addRgType :: (Functor m, MonadIO m)
  => String -> ReleaseGroupType a -> MusicBrainzT m (Entity (ReleaseGroupType a))
addRgType rgTable rgType = head <$> query sql rgType
  where sql = fromString $ unlines
                [ "INSERT INTO " ++ rgTable ++ " (name)"
                , " VALUES (?) RETURNING id, name"
                ]

resolveRgType :: (Functor m, MonadIO m)
  => String -> RefSpec (ReleaseGroupType a) -> MusicBrainzT m (Maybe (Ref (ReleaseGroupType a)))
resolveRgType rgTable rgTypeId = listToMaybe . map fromOnly <$> query sql (Only rgTypeId)
  where sql = fromString $ "SELECT id FROM " ++ rgTable ++ " WHERE id = ?"
