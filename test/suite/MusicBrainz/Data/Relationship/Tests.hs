{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Relationship.Tests ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz hiding (ArtistTree, LabelTree, RecordingTree, ReleaseTree, ReleaseGroupTree, UrlTree, WorkTree)
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor
import MusicBrainz.Lens
import qualified MusicBrainz.Data.Relationship as Relationship

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddRelationshipType
        , testRelationships
        ]

--------------------------------------------------------------------------------
testAddRelationshipType :: Test
testAddRelationshipType = testCase "Can add RelationshipTypes" $ do
  CommonTests.testAdd RelationshipType { relName = "member of" }


--------------------------------------------------------------------------------
-- This will let us create a cartesian product of trees for the test
type TreeCreator a = Ref Editor -> MusicBrainz (Tree a)

data ATree = ArtistTree (TreeCreator Artist)
           | LabelTree (TreeCreator Label)
           | RecordingTree (TreeCreator Recording)
           | ReleaseTree (TreeCreator Release)
           | ReleaseGroupTree (TreeCreator ReleaseGroup)
           | UrlTree (TreeCreator Url)
           | WorkTree (TreeCreator Work)

-- This is just used to generate the test case name
treeType :: ATree -> String
treeType (ArtistTree _) = "artist"
treeType (LabelTree _) = "label"
treeType (RecordingTree _) = "recording"
treeType (ReleaseTree _) = "release"
treeType (ReleaseGroupTree _) = "release group"
treeType (UrlTree _) = "url"
treeType (WorkTree _) = "work"

-- These are entities we will create relationships between
testTrees :: [ATree]
testTrees = [ ArtistTree $ const (return $ minimalTree portishead)
            , LabelTree $ const (return revolutionRecords)
            , RecordingTree mysterons
            , ReleaseTree dummyReleaseTree
            , ReleaseGroupTree dummyReleaseGroupTree
            , UrlTree $ const (return musicBrainz)
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
    go (ArtistTree l) (UrlTree r)          = runTest ArtistRelationship l UrlRelationship r
    go (ArtistTree l) (WorkTree r)         = runTest ArtistRelationship l WorkRelationship r

    go (LabelTree l) (LabelTree r)        = runTest LabelRelationship l LabelRelationship r
    go (LabelTree l) (RecordingTree r)    = runTest LabelRelationship l RecordingRelationship r
    go (LabelTree l) (ReleaseTree r)      = runTest LabelRelationship l ReleaseRelationship r
    go (LabelTree l) (ReleaseGroupTree r) = runTest LabelRelationship l ReleaseGroupRelationship r
    go (LabelTree l) (UrlTree r)          = runTest LabelRelationship l UrlRelationship r
    go (LabelTree l) (WorkTree r)         = runTest LabelRelationship l WorkRelationship r

    go (RecordingTree l) (RecordingTree r)    = runTest RecordingRelationship l RecordingRelationship r
    go (RecordingTree l) (ReleaseTree r)      = runTest RecordingRelationship l ReleaseRelationship r
    go (RecordingTree l) (ReleaseGroupTree r) = runTest RecordingRelationship l ReleaseGroupRelationship r
    go (RecordingTree l) (UrlTree r)          = runTest RecordingRelationship l UrlRelationship r
    go (RecordingTree l) (WorkTree r)         = runTest RecordingRelationship l WorkRelationship r

    go (ReleaseTree l) (ReleaseTree r)      = runTest ReleaseRelationship l ReleaseRelationship r
    go (ReleaseTree l) (ReleaseGroupTree r) = runTest ReleaseRelationship l ReleaseGroupRelationship r
    go (ReleaseTree l) (UrlTree r)          = runTest ReleaseRelationship l UrlRelationship r
    go (ReleaseTree l) (WorkTree r)         = runTest ReleaseRelationship l WorkRelationship r

    go (ReleaseGroupTree l) (ReleaseGroupTree r) = runTest ReleaseGroupRelationship l ReleaseGroupRelationship r
    go (ReleaseGroupTree l) (UrlTree r)          = runTest ReleaseGroupRelationship l UrlRelationship r
    go (ReleaseGroupTree l) (WorkTree r)         = runTest ReleaseGroupRelationship l WorkRelationship r

    go (UrlTree l) (UrlTree r)  = runTest UrlRelationship l UrlRelationship r
    go (UrlTree l) (WorkTree r) = runTest UrlRelationship l WorkRelationship r

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

      changedA <- findLatest (coreRef a)
      changedB <- findLatest (coreRef b)

      edit2 <- createEdit $
        update editor (coreRevision $ changedB) right

      apply edit2

      relationshipsChanged changedA (Set.singleton $ rc (coreRef b) rel) mempty
      relationshipsChanged changedB (Set.singleton $ lc (coreRef a) rel) mempty

      where
        expectedRel =
          Relationship <$> fmap entityRef (add $ RelationshipType "performer")
                       <*> pure mempty
                       <*> pure emptyDate
                       <*> pure emptyDate
                       <*> pure False

        relationshipsChanged for old new = do
          latest <- findLatest (coreRef for)
          oldRels <- Relationship.viewRelationships (coreRevision for)
          newRels <- Relationship.viewRelationships (coreRevision latest)

          liftIO $ do
            oldRels @?= old
            newRels @?= new

