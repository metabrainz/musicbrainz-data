module MusicBrainz.Types.Tests
    ( tests ) where

import Control.Applicative
import Data.Maybe (isNothing)
import Test.MusicBrainz

import qualified Data.UUID as UUID

import MusicBrainz.Types

instance Arbitrary UUID.UUID where
  arbitrary = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (MBID a) where
  arbitrary = MBID <$> arbitrary

tests :: [Test]
tests = [ testGroup "MBID"
            [ testParseMbid
            , testParseInvalidMbid
            , testMbidParseDisplay
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


testMbidParseDisplay :: Test
testMbidParseDisplay = testProperty "parseMbid (mbidToString m) = Just m" $
  \mbid -> parseMbid (mbidToString mbid) == Just mbid

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
