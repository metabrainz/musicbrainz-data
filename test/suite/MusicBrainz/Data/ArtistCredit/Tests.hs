{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ArtistCredit.Tests where

import Control.Applicative
import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.ArtistCredit
import MusicBrainz.Data.Editor

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testExpandCredits ]


--------------------------------------------------------------------------------
testExpandCredits :: Test
testExpandCredits = testCase "Can expand created artist credits" $ do
  editor <- entityRef <$> register acid2
  (a, b) <- autoEdit $
    (,) <$> (fmap coreRef $ create editor (minimalTree portishead) >>= viewRevision)
        <*> (fmap coreRef $ create editor freddie >>= viewRevision)
  let expected = [ArtistCreditName a "A" " & ", ArtistCreditName b "B" ""]
  created <- getRef expected
  actual <- expandCredits $ Set.singleton created
  liftIO $ actual @?= Map.singleton created expected

