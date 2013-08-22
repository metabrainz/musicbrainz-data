module MusicBrainz.Types.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens hiding (elements)
import Data.Maybe (isNothing)
import Test.MusicBrainz

import qualified Data.UUID as UUID

import MusicBrainz.MBID
import MusicBrainz.PartialDate

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
  \uuid -> (UUID.toString uuid) ^? mbid == Just (MBID uuid)


testParseInvalidMbid :: Test
testParseInvalidMbid = testProperty "Non UUID strings do not parse" $
  \s -> isNothing (UUID.fromString s) ==> s ^? mbid == Nothing


testMbidParseDisplay :: Test
testMbidParseDisplay = testProperty "parseMbid (mbidToString m) = Just m" $
  \validMbid -> validMbid ^. re mbid ^? mbid == Just validMbid

--------------------------------------------------------------------------------
newtype Year = Year (Maybe Int) deriving (Show)
newtype Month = Month (Maybe Int) deriving (Show)
newtype Day = Day (Maybe Int) deriving (Show)

instance Arbitrary Year where arbitrary = Year <$> arbitrary

instance Arbitrary Month where
  arbitrary = Month <$> oneof [ return Nothing
                              , Just <$> elements [1..12]
                              ]

instance Arbitrary Day where
  arbitrary = Day <$> oneof [ return Nothing
                            , Just <$> elements [1..31]
                            ]

testNonEmptyPartialDates :: Test
testNonEmptyPartialDates =
  testProperty "Given any partial date with at least one component, the date is not empty" $
    \(Year y) (Month m) (Day d) -> any (not . isNothing) [y, m, d] ==>
      not (isEmpty $ (y, m, d) ^?! partialDate)

testEmptyPartialDates :: Test
testEmptyPartialDates =
  testCase "When all fields are Nothing, the date is empty" $
    assertBool "emptyPartial date is empty"
      (isEmpty $ (Nothing, Nothing, Nothing) ^?! partialDate)
