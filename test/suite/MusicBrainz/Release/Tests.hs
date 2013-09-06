{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Release.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import Test.MusicBrainz
import Test.MusicBrainz.Repository (dummyReleaseTree, acid2, revolutionRecords, mysterons)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.ArtistCredit
import MusicBrainz.Monad
import MusicBrainz.Recording (recordingArtistCredit)
import MusicBrainz.Release
import MusicBrainz.Util (viewOnce)
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testReleaseLabels
        , testTrackLists
        , testCdTocs
        , testMerge
        , testResolveRevisionReference
        , testFindByLabel
        , testFindByReleaseGroup
        , testFindByArtist
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when release exists" $
  CommonTests.testCreateFindLatest dummyReleaseTree


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation dummyReleaseTree


--------------------------------------------------------------------------------
testReleaseLabels :: Test
testReleaseLabels = testCase "Releases can have release labels" $ do
  editor <- entityRef <$> register acid2 { editorName = "warp" }
  revRecLabel <- coreRef <$> autoEdit (create editor revolutionRecords >>= viewRevision)
  let revRec = ReleaseLabel { releaseLabel = Just revRecLabel, releaseCatalogNumber = Just "REVREC001" }

  CommonTests.createAndUpdateSubtree
    dummyReleaseTree
    (releaseLabelsLens .~ Set.singleton revRec)
    releaseLabels
    (\r -> (Map.! r) <$> viewReleaseLabels (Set.singleton r))

  where
    releaseLabelsLens f t = f (releaseLabels t) <&> \b -> t { releaseLabels = b }


--------------------------------------------------------------------------------
testTrackLists :: Test
testTrackLists = testCase "Releases can have track lists" $ do
  createReleaseTreeTest makeMediums


--------------------------------------------------------------------------------
testCdTocs :: Test
testCdTocs = testCase "Releases can have CDTOCs" $
  createReleaseTreeTest $ \editor -> do
    [m] <- makeMediums editor
    return . pure $
       m { mediumCdTocs = Set.singleton $
             CdToc
               (V.fromList [ 150, 31615, 67600, 87137, 108242
                           , 127110, 142910, 166340, 231445 ])
               252000
         }


--------------------------------------------------------------------------------
createReleaseTreeTest :: (Ref Editor -> MusicBrainz [Medium]) -> MusicBrainz ()
createReleaseTreeTest make = do
  editor <- entityRef <$> register acid2

  mediums <- make editor
  tree <- (\t -> t { releaseMediums = mediums }) <$> dummyReleaseTree editor

  release <- autoEdit $ create editor tree >>= fmap coreRevision . viewRevision
  createdMediums <- (Map.! release) <$> viewMediums (Set.singleton release)

  liftIO $ createdMediums @?= mediums


--------------------------------------------------------------------------------
makeMediums :: Ref Editor -> MusicBrainz [Medium]
makeMediums editor = do
  t <- mysterons editor
  mystRec <- autoEdit $ create editor t >>= viewRevision
  return $ [ Medium { mediumName = "Live"
                    , mediumFormat = Nothing
                    , mediumPosition = 1
                    , mediumCdTocs = mempty
                    , mediumTracks = [ Track { trackName = "Mysterons"
                                             , trackRecording = coreRef mystRec
                                             , trackDuration = Nothing
                                             , trackArtistCredit = (recordingArtistCredit (coreData mystRec))
                                             , trackPosition = "1"
                                             } ] } ]

--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct releases" $ do
  CommonTests.testMerge createRecordings
  where
    createRecordings editor = do
      a <- dummyReleaseTree editor
      return (a, modTree a)
    modTree t = t { releaseData = (releaseData t) { releaseName = "Blue Lines" } }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference dummyReleaseTree


--------------------------------------------------------------------------------
testFindByLabel :: Test
testFindByLabel = testCase "Can find releases by their label" $ do
  editor <- entityRef <$> register acid2
  releaseTree <- dummyReleaseTree editor

  (labelId, expected) <- autoEdit $ do
    labelId <- coreRef <$> (create editor revolutionRecords >>= viewRevision)
    let rLabels = Set.singleton $
          ReleaseLabel { releaseLabel = Just labelId
                       , releaseCatalogNumber = Nothing
                       }
    release <- create editor releaseTree { releaseLabels = rLabels } >>= viewRevision
    return (labelId, release)

  releases <- findByLabel labelId
  releases @?= [expected]


--------------------------------------------------------------------------------
testFindByReleaseGroup :: Test
testFindByReleaseGroup = testCase "Can find releases by their release group" $ do
  editor <- entityRef <$> register acid2
  releaseTree <- dummyReleaseTree editor

  expected <- autoEdit $
    create editor releaseTree >>= viewRevision

  releases <- findByReleaseGroup (releaseReleaseGroup . releaseData $ releaseTree)
  releases @?= [expected]


--------------------------------------------------------------------------------
testFindByArtist :: Test
testFindByArtist = testCase "Can find releases by their artist" $ do
  editor <- entityRef <$> register acid2
  releaseTree <- dummyReleaseTree editor

  expected <- autoEdit $
    create editor releaseTree >>= viewRevision

  artist <- acnArtist . head <$> viewOnce expandCredits
    (releaseArtistCredit . releaseData $ releaseTree)

  releases <- findByArtist artist
  releases @?= [expected]
