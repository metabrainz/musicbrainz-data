{-# LANGUAGE TupleSections #-}
module MusicBrainz.Util (groupMap, groupMapTotal, groupRows, viewOnce) where

import Control.Arrow ((&&&))
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (groupBy)
import Data.Monoid (Monoid, mempty)

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
groupRows :: (Eq k, Monoid v) => (a -> (k, v)) -> [a] -> [(k, v)]
groupRows splitRow =
  map (fst . head &&& foldMap snd) .
    groupBy ((==) `on` fst) .
      map splitRow


--------------------------------------------------------------------------------
groupMap :: (Eq k, Monoid v, Ord k) => (a -> (k, v)) -> [k] -> [a] -> Map.Map k v
groupMap f keys = (flip Map.union) (Map.fromList $ map (, mempty) keys) .
  groupMapTotal f


--------------------------------------------------------------------------------
groupMapTotal :: (Eq k, Monoid v, Ord k) => (a -> (k, v)) -> [a] -> Map.Map k v
groupMapTotal f = Map.fromList . groupRows f


--------------------------------------------------------------------------------
viewOnce :: (Functor f, Ord k) => (Set.Set k -> f (Map.Map k v)) -> k -> f v
viewOnce f r = fmap (Map.! r) (f $ Set.singleton r)
