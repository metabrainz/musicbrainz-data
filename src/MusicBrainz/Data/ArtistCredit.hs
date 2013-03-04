{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-| Functions to manipulate 'ArtistCredit's. -}
module MusicBrainz.Data.ArtistCredit
    ( expandCredits
    , getRef
    , allArtistCredits
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate, intersperse, nub)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Database.PostgreSQL.Simple ((:.)(..), In(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Util (groupMapTotal)

--------------------------------------------------------------------------------
data AcParam = AcParam !(Ref Artist) !Int !Int !Text
newtype AcParams = AcParams { getAcParams :: [AcParam] }

instance ToRow AcParam where
  toRow (AcParam artist pos name join) = [ toField artist
                                         , toField pos
                                         , toField name
                                         , toField join
                                         ]

instance ToRow AcParams where
  toRow (AcParams ps) = concatMap toRow ps

{-| Attempt to find a specific 'ArtistCredit' that consists of a given list of
'ArtistCreditName's. If this 'ArtistCredit' does not already exist, create it
and then return a reference to it. -}
getRef :: (Functor m, Monad m, MonadIO m)
  => [ArtistCreditName] -> MusicBrainzT m (Ref ArtistCredit)
getRef acs = do
  nameMap <- Map.fromList <$> returning "SELECT name, find_or_insert_artist_name(name) FROM (VALUES (?)) names (name)" (nub $ map (Only . acnName) acs)

  existing <- fmap fromOnly . listToMaybe <$> query q (parameters nameMap)

  case existing of
    Just r -> return r
    Nothing -> do
      id' <- fromOnly . head <$> query_ "INSERT INTO artist_credit DEFAULT VALUES RETURNING artist_credit_id"
      executeMany "INSERT INTO artist_credit_name (artist_credit_id, artist_id, position, name, join_phrase) VALUES (?, ?, ?, ?, ?)" $
        map (Only id' :.) (getAcParams $ parameters nameMap)
      return id'

  where
    q = fromString $
      unwords [ "SELECT DISTINCT artist_credit_id FROM artist_credit"
              , unwords joins
              , "WHERE"
              , unwords $ intersperse " AND " predicates
              ]

    parameters nameMap = AcParams (map param1 positionedAcs)
      where
        param1 (i, ArtistCreditName {..}) = AcParam acnArtist i (nameMap Map.! acnName) acnJoinPhrase

    joins = map join1 positionedAcs
      where
        join1 (i, _) =
          let alias = acnAlias i
          in "JOIN artist_credit_name " ++ alias ++ " USING (artist_credit_id)"

    predicates = map pred1 positionedAcs
      where
        pred1 (i, _) =
          let alias col = acnAlias i ++ "." ++ col ++ " = ?"
          in intercalate " AND " [ alias "artist_id"
                                 , alias "position"
                                 , alias "name"
                                 , alias "join_phrase"
                                 ]

    positionedAcs = zip [1..] acs

    acnAlias i = "acn_" ++ show i


--------------------------------------------------------------------------------
expandCredits :: (Functor m, MonadIO m)
  => Set.Set (Ref ArtistCredit)
  -> MusicBrainzT m (Map.Map (Ref ArtistCredit) [ArtistCreditName])
expandCredits acIds =
    groupMapTotal partitionArtistCredit <$> query
      [sql| SELECT artist_credit_id, artist_id, name.name, join_phrase
            FROM artist_credit_name
            JOIN artist_name name ON (name.id = artist_credit_name.name)
            WHERE artist_credit_id IN ?
            ORDER BY artist_credit_id, position ASC
      |] (Only . In . Set.toList $ acIds)


partitionArtistCredit :: (Ref ArtistCredit, Ref Artist, Text, Text)
                      -> (Ref ArtistCredit, [ArtistCreditName])
partitionArtistCredit (acId, artistId, name, joinPhrase) =
  (acId, [ArtistCreditName artistId name joinPhrase])


--------------------------------------------------------------------------------
instance ResolveReference ArtistCredit where
  resolveReference acId = listToMaybe . map fromOnly <$>
    query [sql| SELECT artist_credit_id
                FROM artist_credit
                WHERE artist_credit_id = ?
              |]
      (Only acId)


--------------------------------------------------------------------------------
allArtistCredits :: (Functor m, MonadIO m) =>
  Ref Artist -> MusicBrainzT m [[ArtistCreditName]]
allArtistCredits artistId =
    Map.elems . groupMapTotal partitionArtistCredit <$> query q (Only artistId)
  where
    q = [sql| SELECT artist_credit_id, artist_id, name.name, join_phrase
              FROM (
                SELECT artist_credit_id
                FROM (
                  SELECT DISTINCT artist_credit_id
                  FROM recording_data
                  JOIN recording_tree USING (recording_data_id)
                  JOIN recording_revision USING (recording_tree_id)
                  JOIN recording USING (recording_id)
                  WHERE master_revision_id = revision_id

                  UNION

                  SELECT DISTINCT artist_credit_id
                  FROM release_group_data
                  JOIN release_group_tree USING (release_group_data_id)
                  JOIN release_group_revision USING (release_group_tree_id)
                  JOIN release_group USING (release_group_id)
                  WHERE master_revision_id = revision_id

                  UNION

                  SELECT DISTINCT artist_credit_id
                  FROM release_data
                  JOIN release_tree USING (release_data_id)
                  JOIN release_revision USING (release_tree_id)
                  JOIN release USING (release_id)
                  WHERE master_revision_id = revision_id

                  UNION

                  SELECT DISTINCT artist_credit_id
                  FROM track
                  JOIN medium USING (tracklist_id)
                  JOIN release_revision USING (release_tree_id)
                  JOIN release USING (release_id)
                  WHERE master_revision_id = revision_id
                ) q
                JOIN artist_credit_name USING (artist_credit_id)
                WHERE artist_credit_name.artist_id = ?
              ) acn
              JOIN artist_credit_name USING (artist_credit_id)
              JOIN artist_name name ON (name.id = artist_credit_name.name)
              ORDER BY artist_credit_id, position ASC
          |]
