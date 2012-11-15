module MusicBrainz.Data.ClassTests
    ( testCreateFindLatest ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Editor (register)

testCreateFindLatest :: (Eq a, Show a, FindLatest a, Create a)
  => Tree a -> MusicBrainzT IO ()
testCreateFindLatest tree = do
  editor <- register acid2
  created <- create (entityRef editor) tree
  found <- findLatest (coreRef created)
  liftIO $ found @?= created
