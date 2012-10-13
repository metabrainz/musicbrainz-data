{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz
    ( -- * Re-export the testframework
      module Test.Framework
    , module Test.Framework.Providers.HUnit
    , module Test.Framework.Providers.QuickCheck2
    , module Test.HUnit
    , module Test.QuickCheck

      -- * Test utilities
    , mbTest
    , assertException
    ) where


import Control.Exception
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck hiding (Testable)

import MusicBrainz

mbTest :: MusicBrainz a -> IO a
mbTest = runMb databaseSettings
  where databaseSettings = defaultConnectInfo { connectDatabase = "musicbrainz_nes"
                                              , connectUser = "musicbrainz"
                                              }

assertException :: (Exception e, Eq e) => (e -> Maybe Bool) -> IO a -> IO ()
assertException isWanted action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "No exception thrown"

