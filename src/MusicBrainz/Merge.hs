{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-| How to merge various types of data, with the possibility of conflicts. -}
module MusicBrainz.Merge
    ( runMerge, mergeEq, Mergeable(..) ) where

import Control.Applicative
import Data.Functor.Compose

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
data MergeScope a = MergeScope { new :: a, current :: a, ancestor :: a }

instance Functor MergeScope where
  fmap f m = MergeScope { new = f $ new m
                        , current = f $ current m
                        , ancestor = f $ ancestor m
                        }

--------------------------------------------------------------------------------
{-| Merge actions take a 'MergeScope' (of type 'e'), and attempt to merge all 3
sides into a single value of type 'a'. The merge has the option to fail, which
will fail any larger merges that this merge is part of. -}
newtype Merge e a = Merge { getMerge :: Compose ((->) (MergeScope e)) Maybe a }
  deriving (Functor, Applicative)


--------------------------------------------------------------------------------
{-| The simplest merge strategy, which only succeeds if 2 of the sides agree. -}
mergeEq :: Eq a => Merge a a
mergeEq = Merge $ Compose go
  where
    go MergeScope{..}
      | current == ancestor = Just new
      | new == ancestor     = Just current
      | new == current      = Just current
      | otherwise           = Nothing


--------------------------------------------------------------------------------
{-| Attempt to run a merge. If the merge fails due to conflicts, then 'Nothing'
is returned, otherwise 'Just' the merged value is returned. -}
runMerge :: e -> e -> e -> Merge e a -> Maybe a
runMerge new current ancestor (Merge (Compose m)) =
  m (MergeScope new current ancestor)

{-| Given lens to go from the current merge environment, to a component of it,
and a merge strategy to apply those elements, return a merge strategy that is
valid in the current environment.

Less technically, this lets you merge values inside a larger structure (for
example, to merge records). -}
mergedVia :: (e -> e') -> Merge e' a -> Merge e a
mergedVia l m = Merge $ Compose $ (getCompose $ getMerge m) . fmap l

--------------------------------------------------------------------------------
{-| The 'Mergeable' class lets you define a merge strategy for a specific
type. -}
class Mergeable a where
  merge :: Merge a a


instance Mergeable (Tree Artist) where
  merge =
    ArtistTree <$> artistData `mergedVia` mergeArtistData
               <*> artistRelationships `mergedVia` merge
               <*> artistAliases `mergedVia` merge
    where
      mergeArtistData =
        Artist <$> artistName `mergedVia` mergeEq
               <*> artistSortName `mergedVia` mergeEq
               <*> artistComment `mergedVia` mergeEq
               <*> artistBeginDate `mergedVia` mergeEq
               <*> artistEndDate `mergedVia` mergeEq
               <*> artistEnded `mergedVia` mergeEq
               <*> artistGender `mergedVia` mergeEq
               <*> artistType `mergedVia` mergeEq
               <*> artistCountry `mergedVia` mergeEq


instance Ord a => Mergeable (Set.Set a) where
  merge = Merge $ Compose go
    where
      go MergeScope{..} =
        let removed = ancestor `Set.difference` current
            added = current `Set.difference` ancestor
        in Just $ new `Set.difference` (removed `Set.intersection` added)
