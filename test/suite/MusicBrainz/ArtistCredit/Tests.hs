{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.ArtistCredit.Tests where

import Control.Applicative
import Control.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository

import MusicBrainz.Artist
import MusicBrainz.ArtistCredit
import MusicBrainz.Class.Create
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Editor
import MusicBrainz.Entity

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testExpandCredits ]


--------------------------------------------------------------------------------
testExpandCredits :: Test
testExpandCredits = testCase "Can expand created artist credits" $ do
  editor <- entityRef <$> register acid2

  [a1, a2, a3] <- autoEdit $
    mapM (fmap coreRef . (create editor >=> viewRevision))
      [ minimalTree portishead
      , freddie
      , minimalTree portishead { artistName = "Actually not Portishead" }
      ]

  let ac1  = [ArtistCreditName a1 "A" " & ", ArtistCreditName a2 "B" ""]
  let ac2  = [ArtistCreditName a3 "A" "!"]

  [ac1_id, ac2_id] <- mapM getRef [ac1, ac2]

  actual <- expandCredits $ Set.fromList [ ac1_id, ac2_id ]
  actual @?= Map.fromList
    [ (ac1_id, ac1)
    , (ac2_id, ac2)
    ]

