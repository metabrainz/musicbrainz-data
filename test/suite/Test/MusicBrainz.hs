{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz
    ( -- * Re-export test modules
      module Control.Monad.IO.Class
    , module Test.QuickCheck

      -- * Running tests
    , TestEnvironment(..)
    , execTest

      -- * Test utilities
    , testCase
    , testGroup
    , testProperty
    , assertBool
    , assertException
    , Test
    , (@?=)
    , (@?)

      -- * Edit combinators
    , autoEdit
    ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Test.Framework (TestName)
import Test.QuickCheck hiding (Testable)

import qualified Test.HUnit
import qualified Test.Framework
import qualified Test.Framework.Providers.HUnit
import qualified Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck

import MusicBrainz
import MusicBrainz.Data.Edit

data TestEnvironment = TestEnvironment { testContext :: Context }

type Test = ReaderT TestEnvironment IO (Test.Framework.Test)

execTest :: Test -> TestEnvironment -> IO Test.Framework.Test
execTest (ReaderT test) env = test env

assertException :: (Exception e, Eq e, Functor m, MonadCatchIO m) => (e -> Maybe b) -> m a -> m ()
assertException isWanted action =
  tryJust isWanted action >>=
    either (const $ return ()) (const $ liftIO $ Test.HUnit.assertFailure "No exception thrown")

autoEdit :: EditM a -> MusicBrainz a
autoEdit action = do
  editId <- openEdit
  withEdit editId action <* apply editId

testGroup :: TestName -> [Test] -> Test
testGroup groupName = fmap (Test.Framework.testGroup groupName) . sequence

testCase :: TestName -> MusicBrainz a -> Test
testCase testName test = do
  ctx <- asks testContext
  return $ Test.Framework.Providers.HUnit.testCase testName $
    void $ runMbContext ctx $ withTransactionRollBack test

testProperty :: Test.QuickCheck.Testable a => TestName -> a -> Test
testProperty name p = return $ Test.Framework.Providers.QuickCheck2.testProperty name p

(@?=) :: (Eq a, MonadIO m, Show a) => a -> a -> m ()
a @?= b = liftIO (a Test.HUnit.@?= b)

(@?) :: (MonadIO m, Test.HUnit.AssertionPredicable t) => t -> String -> m ()
a @? msg = liftIO (a Test.HUnit.@? msg)

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool message predicate = liftIO (Test.HUnit.assertBool message predicate)
