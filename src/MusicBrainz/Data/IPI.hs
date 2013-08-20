{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Functions for working with IPI codes. -}
module MusicBrainz.Data.IPI
    ( ViewIPICodes(..) ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Database.PostgreSQL.Simple (Only(..), In(..))

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.CoreEntity (CoreEntityTable(..))
import MusicBrainz.Data.Util (groupMap)

--------------------------------------------------------------------------------
{-| Implemented by types that can have IPI codes in their tree, and provides
methods to work with those IPI codes. -}
class ViewIPICodes a where
  {-| Fetch all IPI codes for a set of revisions. -}
  viewIpiCodes :: (Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set IPI))

  default viewIpiCodes
    :: (CoreEntityTable a, Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set IPI))
  viewIpiCodes rs = groupMap partitionIpis rs' <$> query q (Only . In $ rs')
   where
    entityName = untag (rootTable :: Tagged a String)
    rs' = Set.toList rs
    partitionIpis (revisionId, ipiCode) = (revisionId, Set.singleton ipiCode)
    q = fromString $ unlines
          [ "SELECT revision_id, ipi "
          , "FROM " ++ entityName ++ "_ipi "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]
