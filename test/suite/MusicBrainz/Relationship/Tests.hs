{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Relationship.Tests ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Util (viewOnce)
import MusicBrainz.Monad
import MusicBrainz.Class.Create
import MusicBrainz.Class.FindLatest
import MusicBrainz.Class.Update
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit
import MusicBrainz.EditApplication
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.PartialDate
import MusicBrainz.Relationship
import MusicBrainz.Tree
import MusicBrainz.Recording hiding (RecordingTree)
import MusicBrainz.Release hiding (ReleaseTree)
import MusicBrainz.ReleaseGroup hiding (ReleaseGroupTree)
import MusicBrainz.Artist hiding (ArtistTree)
import MusicBrainz.URL hiding (URLTree)
import MusicBrainz.Work hiding (WorkTree)
import MusicBrainz.Label hiding (LabelTree)
import MusicBrainz.Ref

import qualified MusicBrainz.Relationship.Internal as Relationship

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddRelationshipType
        , testResolveRelationshipTypeReference
        , testResolveRelationshipAttributeReference
        , testRelationships
        ]

--------------------------------------------------------------------------------
testAddRelationshipType :: Test
testAddRelationshipType = testCase "Can add RelationshipTypes" $ do
  CommonTests.testAdd performer


--------------------------------------------------------------------------------
testResolveRelationshipTypeReference :: Test
testResolveRelationshipTypeReference = testCase "Can resolve RelationshipTypes" $ do
  CommonTests.testResolveReference (add performer) entityRef


--------------------------------------------------------------------------------
testResolveRelationshipAttributeReference :: Test
testResolveRelationshipAttributeReference = testCase "Can resolve RelationshipAttributes" $ do
  CommonTests.testResolveReference additional entityRef


--------------------------------------------------------------------------------
-- This will let us create a cartesian product of trees for the test
type TreeCreator a = Ref Editor -> MusicBrainz (Tree a)

data ATree = ArtistTree (TreeCreator Artist)
           | LabelTree (TreeCreator Label)
           | RecordingTree (TreeCreator Recording)
           | ReleaseTree (TreeCreator Release)
           | ReleaseGroupTree (TreeCreator ReleaseGroup)
           | URLTree (TreeCreator URL)
           | WorkTree (TreeCreator Work)

-- This is just used to generate the test case name
treeType :: ATree -> String
treeType (ArtistTree _) = "artist"
treeType (LabelTree _) = "label"
treeType (RecordingTree _) = "recording"
treeType (ReleaseTree _) = "release"
treeType (ReleaseGroupTree _) = "release group"
treeType (URLTree _) = "url"
treeType (WorkTree _) = "work"

-- These are entities we will create relationships between
testTrees :: [ATree]
testTrees = [ ArtistTree $ const (return $ minimalTree portishead)
            , LabelTree $ const (return revolutionRecords)
            , RecordingTree mysterons
            , ReleaseTree dummyReleaseTree
            , ReleaseGroupTree dummyReleaseGroupTree
            , URLTree $ const (return musicBrainz)
            , WorkTree $ const (return wildRose)
            ]

testRelationships :: Test
testRelationships = testGroup "Can have relationships between core entities"
    [ testCase (treeType left ++ "-" ++ treeType right) $ go left right
        | left <- testTrees
        , right <- testTrees
        , treeType right >= treeType left
    ]
  where
    go (ArtistTree l) (ArtistTree r)       = runTest ArtistRelationship l ArtistRelationship r
    go (ArtistTree l) (LabelTree r)        = runTest ArtistRelationship l LabelRelationship r
    go (ArtistTree l) (RecordingTree r)    = runTest ArtistRelationship l RecordingRelationship r
    go (ArtistTree l) (ReleaseTree r)      = runTest ArtistRelationship l ReleaseRelationship r
    go (ArtistTree l) (ReleaseGroupTree r) = runTest ArtistRelationship l ReleaseGroupRelationship r
    go (ArtistTree l) (URLTree r)          = runTest ArtistRelationship l URLRelationship r
    go (ArtistTree l) (WorkTree r)         = runTest ArtistRelationship l WorkRelationship r

    go (LabelTree l) (LabelTree r)        = runTest LabelRelationship l LabelRelationship r
    go (LabelTree l) (RecordingTree r)    = runTest LabelRelationship l RecordingRelationship r
    go (LabelTree l) (ReleaseTree r)      = runTest LabelRelationship l ReleaseRelationship r
    go (LabelTree l) (ReleaseGroupTree r) = runTest LabelRelationship l ReleaseGroupRelationship r
    go (LabelTree l) (URLTree r)          = runTest LabelRelationship l URLRelationship r
    go (LabelTree l) (WorkTree r)         = runTest LabelRelationship l WorkRelationship r

    go (RecordingTree l) (RecordingTree r)    = runTest RecordingRelationship l RecordingRelationship r
    go (RecordingTree l) (ReleaseTree r)      = runTest RecordingRelationship l ReleaseRelationship r
    go (RecordingTree l) (ReleaseGroupTree r) = runTest RecordingRelationship l ReleaseGroupRelationship r
    go (RecordingTree l) (URLTree r)          = runTest RecordingRelationship l URLRelationship r
    go (RecordingTree l) (WorkTree r)         = runTest RecordingRelationship l WorkRelationship r

    go (ReleaseTree l) (ReleaseTree r)      = runTest ReleaseRelationship l ReleaseRelationship r
    go (ReleaseTree l) (ReleaseGroupTree r) = runTest ReleaseRelationship l ReleaseGroupRelationship r
    go (ReleaseTree l) (URLTree r)          = runTest ReleaseRelationship l URLRelationship r
    go (ReleaseTree l) (WorkTree r)         = runTest ReleaseRelationship l WorkRelationship r

    go (ReleaseGroupTree l) (ReleaseGroupTree r) = runTest ReleaseGroupRelationship l ReleaseGroupRelationship r
    go (ReleaseGroupTree l) (URLTree r)          = runTest ReleaseGroupRelationship l URLRelationship r
    go (ReleaseGroupTree l) (WorkTree r)         = runTest ReleaseGroupRelationship l WorkRelationship r

    go (URLTree l) (URLTree r)  = runTest URLRelationship l URLRelationship r
    go (URLTree l) (WorkTree r) = runTest URLRelationship l WorkRelationship r

    go (WorkTree l) (WorkTree r) = runTest WorkRelationship l WorkRelationship r

    go l r  = error $ "Unexpected relationship combination: " ++ treeType l ++ "-" ++ treeType r

    runTest lc makeLeft rc makeRight = do
      editor <- entityRef <$> register acid2
      rel <- expectedRel

      left <- makeLeft editor
      right <- makeRight editor

      a <- autoEdit $ create editor left >>= viewRevision
      b <- autoEdit $ create editor right >>= viewRevision

      edit1 <- createEdit $
        update editor (coreRevision a) (left & relationships .~ Set.singleton (rc (coreRef b) rel))

      apply edit1

      relationshipsChanged a mempty (Set.singleton $ rc (coreRef b) rel)
      relationshipsChanged b mempty (Set.singleton $ lc (coreRef a) rel)

      changedA <- viewOnce findLatest (coreRef a)
      changedB <- viewOnce findLatest (coreRef b)

      edit2 <- createEdit $
        update editor (coreRevision $ changedB) right

      apply edit2

      relationshipsChanged changedA (Set.singleton $ rc (coreRef b) rel) mempty
      relationshipsChanged changedB (Set.singleton $ lc (coreRef a) rel) mempty

      where
        expectedRel =
          Relationship <$> fmap entityRef (add performer)
                       <*> pure mempty
                       <*> pure emptyDate
                       <*> pure emptyDate
                       <*> pure False

        relationshipsChanged for old new = do
          latest <- viewOnce findLatest (coreRef for)
          oldRels <- Relationship.viewRelationships (coreRevision for)
          newRels <- Relationship.viewRelationships (coreRevision latest)

          liftIO $ do
            oldRels @?= old
            newRels @?= new

