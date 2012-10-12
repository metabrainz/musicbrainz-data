module MusicBrainz.Types.Tests
    ( tests ) where

import Control.Applicative
import Data.Maybe (isJust)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.UUID as UUID

import MusicBrainz.Types

instance Arbitrary UUID.UUID where
  arbitrary = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

tests :: [Test]
tests = [ testParseMbid ]

--------------------------------------------------------------------------------
{-| Converting a UUID into a string and parsing it produces an MBID, whos
inner UUID is the same as the input. -}
testParseMbid :: Test
testParseMbid = testProperty "Can parse UUIDs as MBIDs" $ do
  \uuid -> parseMbid (UUID.toString uuid) == Just (MBID uuid)
