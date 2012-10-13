module MusicBrainz.Types.Tests
    ( tests ) where

import Control.Applicative
import Data.Maybe (isNothing)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit hiding (Test)

import qualified Data.UUID as UUID

import MusicBrainz.Types

instance Arbitrary UUID.UUID where
  arbitrary = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

tests :: [Test]
tests = [ testGroup "MBID"
            [ testParseMbid
            , testParseInvalidMbid
            ]
        , testGroup "PartialDate"
            [ testEmptyPartialDates
            , testNonEmptyPartialDates
            ]
        ]

--------------------------------------------------------------------------------
testParseMbid :: Test
testParseMbid = testProperty "Can parse UUIDs as MBIDs" $
  \uuid -> parseMbid (UUID.toString uuid) == Just (MBID uuid)


testParseInvalidMbid :: Test
testParseInvalidMbid = testProperty "Non UUID strings do not parse" $
  \s -> isNothing (UUID.fromString s) ==> parseMbid s == Nothing

--------------------------------------------------------------------------------
testNonEmptyPartialDates :: Test
testNonEmptyPartialDates =
  testProperty "When any field is set, the date is not empty" $
    \y m d -> any (not . isNothing) [y, m, d] ==>
                not (isEmpty $ PartialDate y m d)

testEmptyPartialDates :: Test
testEmptyPartialDates =
  testCase "When all fields are Nothing, the date is empty" $
    assertBool "emptyPartial date is empty"
      (isEmpty $ PartialDate Nothing Nothing Nothing)
