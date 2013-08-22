{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-| How to merge various types of data, with the possibility of conflicts. -}
module MusicBrainz.Merge where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Product
import GHC.Exts

import qualified Data.Algorithm.Diff3 as Diff3
import qualified Data.Set as Set

--------------------------------------------------------------------------------
class Render a b where
  render :: MergeScope a -> b

instance Render a () where render = const ()

--------------------------------------------------------------------------------
data MergeScope a = MergeScope { new :: !a, current :: !a, ancestor :: !a }

instance Functor MergeScope where
  fmap f m = MergeScope { new = f $ new m
                        , current = f $ current m
                        , ancestor = f $ ancestor m
                        }


--------------------------------------------------------------------------------
{-| Merge actions take a 'MergeScope' (of type 'e'), and attempt to merge all 3
sides into a single value of type 'a'. The merge has the option to fail, which
will fail any larger merges that this merge is part of. -}
newtype Merge e mo a = Merge { getMerge :: Compose ((->) (MergeScope e)) (Product (Const [Hunk mo]) Maybe) a }
  deriving (Functor, Applicative)


--------------------------------------------------------------------------------
{-| When traversing a merge, hunks will be output. 'Hunk's form a forest, where
nodes can be a 'Section' containing a forest of 'Hunk's, or they can either be
'Ok' or in 'Conflict'. -}
data Hunk a = Section !String ![Hunk a] | Ok !a | Conflict !a
  deriving (Show)

{-| Scope the output forest of 'Hunk's from 'Merge' into a new 'Section' with
a given label. -}
(.>) :: String -> Merge e mo a -> Merge e mo a
label .> (Merge (Compose m)) = Merge $ Compose remap
  where
    remap scope = case m scope of
      Pair (Const hunks) a -> Pair (Const [Section label hunks]) a

infixr 5 .>


--------------------------------------------------------------------------------
{-| The simplest merge strategy, which only succeeds if 2 of the sides agree. -}
mergeEq :: (Eq a, Render a mo) => Merge a mo a
mergeEq = Merge $ Compose go
  where
    go scope@MergeScope{..}
      | current == ancestor = emit scope (Just new)
      | new == ancestor     = emit scope (Just current)
      | new == current      = emit scope (Just current)
      | otherwise           = emit scope Nothing


--------------------------------------------------------------------------------
emit :: Render a mo => MergeScope a -> Maybe a -> Product (Const [Hunk mo]) Maybe a
emit scope (Just v) = Pair (Const [Ok $ render scope]) (Just v)
emit scope Nothing  = Pair (Const [Conflict $ render scope]) Nothing


--------------------------------------------------------------------------------
{-| Attempt to run a merge. If the merge fails due to conflicts, then 'Nothing'
is returned, otherwise 'Just' the merged value is returned. -}
runMerge :: e -> e -> e -> Merge e () a -> Maybe a
runMerge new current ancestor (Merge (Compose m)) =
  case m (MergeScope new current ancestor) of
    Pair _ v -> v

--------------------------------------------------------------------------------
execMerge :: e -> e -> e -> Merge e mo a -> ([Hunk mo], Maybe a)
execMerge new current ancestor (Merge (Compose m)) =
  case m (MergeScope new current ancestor) of
    Pair (Const v) a -> (v, a)


--------------------------------------------------------------------------------
{-| Given lens to go from the current merge environment, to a component of it,
and a merge strategy to apply those elements, return a merge strategy that is
valid in the current environment.

Less technically, this lets you merge values inside a larger structure (for
example, to merge records). -}
mergedVia :: (e -> e') -> Merge e' mo a -> Merge e mo a
mergedVia l m = Merge $ Compose $ getCompose (getMerge m) . fmap l


--------------------------------------------------------------------------------
{-| The 'Mergeable' class lets you define a merge strategy for a specific
type. -}
class MergeRender a () => Mergeable a where
  type MergeRender a mo :: Constraint
  merge :: MergeRender a m => Merge a m a


--------------------------------------------------------------------------------
instance Ord a => Mergeable (Set.Set a) where
  type MergeRender (Set.Set a) m = Render (Set.Set a) m
  merge = Merge $ Compose go
    where
      go scope@MergeScope{..} =
        let removed = ancestor `Set.difference` current
            added = current `Set.difference` ancestor
        in emit scope $ Just $ new `Set.difference` (removed `Set.intersection` added)


--------------------------------------------------------------------------------
instance (Eq a, Mergeable a) => Mergeable [a] where
  type MergeRender [a] mo = (Render [a] mo)
  merge = Merge $ Compose go
    where
      go scope@MergeScope{..} =
        emit scope . sequence . concatMap processHunk $
          Diff3.diff3 new ancestor current

      processHunk (Diff3.Conflict ls os rs)  = resolveConflicts ls os rs
      processHunk (Diff3.Unchanged os)   = map Just os
      processHunk (Diff3.LeftChange ls)  = map Just ls
      processHunk (Diff3.RightChange rs) = map Just rs

      resolveConflicts [] [] []             = []
      resolveConflicts (l:ls) (o:os) (r:rs) = runMerge l r o merge : resolveConflicts ls os rs
      resolveConflicts _ _ _                = [Nothing]
