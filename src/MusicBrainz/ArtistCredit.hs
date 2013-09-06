{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.ArtistCredit where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate, intersperse, nub)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Database.PostgreSQL.Simple ((:.)(..), In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Util (groupMapTotal)
import MusicBrainz.Monad
import MusicBrainz.Artist (Artist)
import MusicBrainz.Versioning

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| An artist credit is an ordered lists of artists, intercalated with free text
strings and with each artist having a a credited name (possibly different from
the actual artists name.

This type is completely uninhabited, and you should work with artist credits
through lists of 'ArtistCreditName'. -}
data ArtistCredit

instance Referenceable ArtistCredit where
  type RefSpec ArtistCredit = Int

instance FromField (Ref ArtistCredit) where
  fromField f v = view reference <$> fromField f v

instance ToField (Ref ArtistCredit) where
  toField = toField . dereference

instance ResolveReference ArtistCredit where
  resolveReference acId = listToMaybe . map fromOnly <$>
    query [sql| SELECT artist_credit_id
                FROM artist_credit
                WHERE artist_credit_id = ?
              |]
      (Only acId)

--------------------------------------------------------------------------------
{-| An individual artist credit in an 'ArtistCredit'. This can also be thought
of as a single 'Artist' appearing in an 'ArtistCredit'. -}
data ArtistCreditName = ArtistCreditName
    { acnArtist :: !(Ref Artist)
    , acnName :: !Text
    , acnJoinPhrase :: !Text
    }
  deriving (Eq, Show)


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


--------------------------------------------------------------------------------
partitionArtistCredit :: (Ref ArtistCredit, Ref Artist, Text, Text)
                      -> (Ref ArtistCredit, [ArtistCreditName])
partitionArtistCredit (acId, artistId, name, joinPhrase) =
  (acId, [ArtistCreditName artistId name joinPhrase])
