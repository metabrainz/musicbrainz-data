{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| Functions to manipulate 'ArtistCredit's. -}
module MusicBrainz.Data.ArtistCredit
    ( getRef ) where

import Control.Applicative
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Database.PostgreSQL.Simple ((:.)(..), Only(..))
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import MusicBrainz

--------------------------------------------------------------------------------
data AcParam = AcParam (Ref Artist) Int Int Text
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
getRef :: [ArtistCreditName] -> MusicBrainz (Ref ArtistCredit)
getRef acs = do
  nameMap <- Map.fromList <$> returning "SELECT name, find_or_insert_artist_name(name) FROM (VALUES (?)) names (name)" (nub $ map (Only . acnName) acs)

  existing <- fmap fromOnly . listToMaybe <$> query sql (parameters nameMap)

  case existing of
    Just r -> return r
    Nothing -> do
      id' <- fromOnly . head <$> query_ "INSERT INTO artist_credit DEFAULT VALUES RETURNING artist_credit_id"
      executeMany "INSERT INTO artist_credit_name (artist_credit_id, artist_id, position, name, join_phrase) VALUES (?, ?, ?, ?, ?)" $
        map (Only id' :.) (getAcParams $ parameters nameMap)
      return id'

  where
    sql = fromString $
      unwords [ "SELECT DISTINCT artist_credit_id FROM artist_credit"
              , unwords joins
              , "WHERE"
              , unwords predicates
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
          in intercalate " AND " $ [ alias "artist_id"
                                   , alias "position"
                                   , alias "name"
                                   , alias "join_phrase"
                                   ]

    positionedAcs = zip [1..] acs

    acnAlias i = "acn_" ++ show i
