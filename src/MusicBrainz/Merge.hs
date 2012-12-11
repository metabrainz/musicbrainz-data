{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-| How to merge various types of data, with the possibility of conflicts. -}
module MusicBrainz.Merge
    ( runMerge, mergeEq, Mergeable(..) ) where

import Control.Applicative
import Data.Functor.Compose

import qualified Data.Set as Set

import MusicBrainz.Types

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
mergedVia l m = Merge $ Compose $ getCompose (getMerge m) . fmap l

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
               <*> artistIpiCodes `mergedVia` merge
               <*> artistAnnotation `mergedVia` mergeEq
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


instance Mergeable (Tree Label) where
  merge =
    LabelTree <$> labelData `mergedVia` mergeLabelData
              <*> labelAliases `mergedVia` merge
              <*> labelIpiCodes `mergedVia` merge
              <*> labelAnnotation `mergedVia` mergeEq
    where
      mergeLabelData =
        Label <$> labelName `mergedVia` mergeEq
              <*> labelSortName `mergedVia` mergeEq
              <*> labelComment `mergedVia` mergeEq
              <*> labelBeginDate `mergedVia` mergeEq
              <*> labelEndDate `mergedVia` mergeEq
              <*> labelEnded `mergedVia` mergeEq
              <*> labelType `mergedVia` mergeEq
              <*> labelCode `mergedVia` mergeEq
              <*> labelCountry `mergedVia` mergeEq


instance Mergeable (Tree Recording) where
  merge =
    RecordingTree <$> recordingData `mergedVia` mergeRecordingData
                  <*> recordingAnnotation `mergedVia` mergeEq
                  <*> recordingIsrcs `mergedVia` merge
                  <*> recordingPuids `mergedVia` merge
    where
      mergeRecordingData =
        Recording
              <$> recordingName `mergedVia` mergeEq
              <*> recordingComment `mergedVia` mergeEq
              <*> recordingArtistCredit `mergedVia` mergeEq
              <*> recordingDuration `mergedVia` mergeEq


instance Mergeable (Tree Release) where
  merge =
    ReleaseTree <$> releaseData `mergedVia` mergeReleaseData
                <*> releaseAnnotation `mergedVia` mergeEq
                <*> releaseLabels `mergedVia` merge
                <*> releaseMediums `mergedVia` mergeEq
    where
      mergeReleaseData =
        Release
              <$> releaseName `mergedVia` mergeEq
              <*> releaseComment `mergedVia` mergeEq
              <*> releaseArtistCredit `mergedVia` mergeEq
              <*> releaseReleaseGroup `mergedVia` mergeEq
              <*> releaseDate `mergedVia` mergeEq
              <*> releaseCountry `mergedVia` mergeEq
              <*> releaseScript `mergedVia` mergeEq
              <*> releaseLanguage `mergedVia` mergeEq
              <*> releasePackaging `mergedVia` mergeEq
              <*> releaseStatus `mergedVia` mergeEq


instance Mergeable (Tree ReleaseGroup) where
  merge =
    ReleaseGroupTree <$> releaseGroupData `mergedVia` mergeReleaseGroupData
                     <*> releaseGroupAnnotation `mergedVia` mergeEq
    where
      mergeReleaseGroupData =
        ReleaseGroup
              <$> releaseGroupName `mergedVia` mergeEq
              <*> releaseGroupComment `mergedVia` mergeEq
              <*> releaseGroupArtistCredit `mergedVia` mergeEq
              <*> releaseGroupPrimaryType `mergedVia` mergeEq
              <*> releaseGroupSecondaryTypes `mergedVia` merge

instance Mergeable (Tree Url) where
  merge =
    UrlTree <$> urlData `mergedVia` mergeUrlData
    where
      mergeUrlData =
        Url <$> urlUrl `mergedVia` mergeEq

instance Mergeable (Tree Work) where
  merge =
    WorkTree <$> workData `mergedVia` mergeWorkData
             <*> workAliases `mergedVia` merge
             <*> workAnnotation `mergedVia` mergeEq
             <*> workIswcs `mergedVia` merge
    where
      mergeWorkData =
        Work
              <$> workName `mergedVia` mergeEq
              <*> workComment `mergedVia` mergeEq
              <*> workType `mergedVia` mergeEq
              <*> workLanguage `mergedVia` mergeEq


instance Ord a => Mergeable (Set.Set a) where
  merge = Merge $ Compose go
    where
      go MergeScope{..} =
        let removed = ancestor `Set.difference` current
            added = current `Set.difference` ancestor
        in Just $ new `Set.difference` (removed `Set.intersection` added)
