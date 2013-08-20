{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Functions for working with ISNI codes. -}
module MusicBrainz.Data.ISNI
    ( ViewISNICodes(..) ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..), In(..))
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Data.Util (groupMap)

--------------------------------------------------------------------------------
{-| Implemented by types that can have ISNI codes in their tree, and provides
methods to work with those ISNI codes. -}
class ViewISNICodes a where
  {-| Fetch all ISNI codes for a set of revisions. -}
  viewIsniCodes :: (Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set ISNI))

  default viewIsniCodes
    :: (Functor m, MonadIO m, CoreEntityTable a)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set ISNI))
  viewIsniCodes rs =
    groupMap partitionIsnis rs' <$> query q (Only . In $ rs')
   where
    entityName = untag (rootTable :: Tagged a String)
    rs' = Set.toList rs
    partitionIsnis (revisionId, isniCode) = (revisionId, Set.singleton isniCode)
    q = fromString $ unlines
          [ "SELECT revision_id, isni "
          , "FROM " ++ entityName ++ "_isni "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]


