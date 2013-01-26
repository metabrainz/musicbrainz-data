{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-| How to merge various types of data, with the possibility of conflicts. -}
module MusicBrainz.Merge
    ( execMerge, Hunk(..), runMerge, mergeEq, Mergeable(..), Merge, Render(..) ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Product
import Data.Text (Text)
import GHC.Exts
import Network.URI (URI)

import qualified Data.Algorithm.Diff3 as Diff3
import qualified Data.Set as Set

import MusicBrainz.Types

--------------------------------------------------------------------------------
class Render a b where
  render :: MergeScope a -> b

instance Render a () where render = const ()

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
newtype Merge e mo a = Merge { getMerge :: Compose ((->) (MergeScope e)) (Product (Const [Hunk mo]) Maybe) a }
  deriving (Functor, Applicative)


--------------------------------------------------------------------------------
{-| When traversing a merge, hunks will be output. 'Hunk's form a forest, where
nodes can be a 'Section' containing a forest of 'Hunk's, or they can either be
'Ok' or in 'Conflict'. -}
data Hunk a = Section String [Hunk a] | Ok a | Conflict a

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
instance Mergeable (Tree Artist) where
  type MergeRender (Tree Artist) mo =
    ( Render (Maybe (Ref ArtistType)) mo
    , Render (Maybe (Ref Country)) mo
    , Render (Maybe (Ref Gender)) mo
    , Render (Set.Set (Alias Artist)) mo
    , Render (Set.Set IPI) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Bool mo
    , Render PartialDate mo
    , Render Text mo
    )

  merge =
    ArtistTree <$> "Artist" .>
                     artistData `mergedVia` mergeArtistData
               <*> "Relationships" .>
                     artistRelationships `mergedVia` merge
               <*> "Aliases" .>
                     artistAliases `mergedVia` merge
               <*> "IPI Codes" .>
                     artistIpiCodes `mergedVia` merge
               <*> "Annotation" .>
                     artistAnnotation `mergedVia` mergeEq
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


--------------------------------------------------------------------------------
instance Mergeable (Tree Label) where
  type MergeRender (Tree Label) mo =
    ( Render (Maybe Int) mo
    , Render (Maybe (Ref LabelType)) mo
    , Render (Maybe (Ref Country)) mo
    , Render (Set.Set (Alias Label)) mo
    , Render (Set.Set IPI) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Bool mo
    , Render PartialDate mo
    , Render Text mo
    )

  merge =
    LabelTree <$> labelData `mergedVia` mergeLabelData
              <*> labelRelationships `mergedVia` merge
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


--------------------------------------------------------------------------------
instance Mergeable Medium where
  type MergeRender Medium mo =
    ( Render Int mo
    , Render (Maybe (Ref MediumFormat)) mo
    , Render (Set.Set CdToc) mo
    , Render [Track] mo
    , Render Text mo
    )
  merge = Medium <$> mediumName `mergedVia` mergeEq
                 <*> mediumFormat `mergedVia` mergeEq
                 <*> mediumPosition `mergedVia` mergeEq
                 <*> mediumTracks `mergedVia` merge
                 <*> mediumCdTocs `mergedVia` merge


--------------------------------------------------------------------------------
instance Mergeable (Tree Recording) where
  type MergeRender (Tree Recording) mo =
    ( Render (Maybe Int) mo
    , Render (Ref ArtistCredit) mo
    , Render (Set.Set ISRC) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render (Set.Set PUID) mo
    , Render Text mo
    )

  merge =
    RecordingTree <$> recordingData `mergedVia` mergeRecordingData
                  <*> recordingRelationships `mergedVia` merge
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


--------------------------------------------------------------------------------
instance Mergeable (Tree Release) where
  type MergeRender (Tree Release) mo =
    ( Render (Maybe (Ref Country)) mo
    , Render (Maybe (Ref Language)) mo
    , Render (Maybe (Ref ReleasePackaging)) mo
    , Render (Maybe (Ref ReleaseStatus)) mo
    , Render (Maybe (Ref Script)) mo
    , Render [Medium] mo
    , Render PartialDate mo
    , Render (Ref ArtistCredit) mo
    , Render (Ref ReleaseGroup) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render (Set.Set ReleaseLabel) mo
    , Render Text mo
    )

  merge =
    ReleaseTree <$> releaseData `mergedVia` mergeReleaseData
                <*> releaseRelationships `mergedVia` merge
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


--------------------------------------------------------------------------------
instance Mergeable (Tree ReleaseGroup) where
  type MergeRender (Tree ReleaseGroup) mo =
    ( Render (Maybe (Ref (ReleaseGroupType Primary))) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render (Set.Set (Ref (ReleaseGroupType Secondary))) mo
    , Render (Ref ArtistCredit) mo
    , Render Text mo
    )

  merge =
    ReleaseGroupTree <$> releaseGroupData `mergedVia` mergeReleaseGroupData
                     <*> releaseGroupRelationships `mergedVia` merge
                     <*> releaseGroupAnnotation `mergedVia` mergeEq
    where
      mergeReleaseGroupData =
        ReleaseGroup
              <$> releaseGroupName `mergedVia` mergeEq
              <*> releaseGroupComment `mergedVia` mergeEq
              <*> releaseGroupArtistCredit `mergedVia` mergeEq
              <*> releaseGroupPrimaryType `mergedVia` mergeEq
              <*> releaseGroupSecondaryTypes `mergedVia` merge


--------------------------------------------------------------------------------
instance Mergeable Track where
  type MergeRender Track mo =
    ( Render (Maybe Int) mo
    , Render (Ref ArtistCredit) mo
    , Render (Ref Recording) mo
    , Render Text mo
    )
  merge = Track <$> trackName `mergedVia` mergeEq
                <*> trackRecording `mergedVia` mergeEq
                <*> trackDuration `mergedVia` mergeEq
                <*> trackArtistCredit `mergedVia` mergeEq
                <*> trackPosition `mergedVia` mergeEq


--------------------------------------------------------------------------------
instance Mergeable (Tree Url) where
  type MergeRender (Tree Url) mo =
    ( Render (Set.Set LinkedRelationship) mo
    , Render URI mo
    )

  merge =
    UrlTree <$> urlData `mergedVia` mergeUrlData
            <*> urlRelationships `mergedVia` merge
    where
      mergeUrlData =
        Url <$> urlUrl `mergedVia` mergeEq


--------------------------------------------------------------------------------
instance Mergeable (Tree Work) where
  type MergeRender (Tree Work) mo =
    ( Render (Maybe (Ref Language)) mo
    , Render (Maybe (Ref WorkType)) mo
    , Render (Set.Set (Alias Work)) mo
    , Render (Set.Set ISWC) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Text mo )

  merge =
    WorkTree <$> workData `mergedVia` mergeWorkData
             <*> workRelationships `mergedVia` merge
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
