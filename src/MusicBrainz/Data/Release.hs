{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Release's in the MusicBrainz database. -}
module MusicBrainz.Data.Release
    ( create ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

instance FindLatest Release where
  findLatest releaseId = listToMaybe <$> query q (Only releaseId)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE release_id = ?
        AND revision_id = master_revision_id  |]

--------------------------------------------------------------------------------
{-| Create an entirely new release, returning the final 'CoreEntity' as it is
in the database. -}
create :: Ref Editor -> Release -> MusicBrainz (CoreEntity Release)
create = GenericCreate.create GenericCreate.Specification
    { GenericCreate.getTree = releaseTree
    , GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "release"
    , GenericCreate.newEntityRevision = newReleaseRevision
    , GenericCreate.linkRevision = linkRevision
    }
  where
    newReleaseRevision releaseId releaseTreeId revisionId = selectValue $
      query [sql| INSERT INTO release_revision (release_id, revision_id, release_tree_id)
                  VALUES (?, ?, ?) RETURNING revision_id |]
        (releaseId, revisionId, releaseTreeId)

    linkRevision releaseId revisionId = void $
      execute [sql| UPDATE release SET master_revision_id = ? WHERE release_id = ? |] (revisionId, releaseId)


--------------------------------------------------------------------------------
releaseTree :: Release -> MusicBrainz (Ref (Tree Release))
releaseTree release = findOrInsertReleaseData >>= findOrInsertReleaseTree
  where
    findOrInsertReleaseData :: MusicBrainz Int
    findOrInsertReleaseData = selectValue $
      query [sql| SELECT find_or_insert_release_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        release

    findOrInsertReleaseTree dataId = selectValue $
      query [sql| SELECT find_or_insert_release_tree(?, ?) |]
        (dataId, releaseReleaseGroup release)

