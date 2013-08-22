{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MusicBrainz.Merge where

import GHC.Exts

data Merge e mo a

runMerge :: e -> e -> e -> Merge e () a -> Maybe a

class MergeRender a () => Mergeable a where
  type MergeRender a mo :: Constraint
  merge :: MergeRender a m => Merge a m a
