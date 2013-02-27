{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz
    ( -- * Re-export test modules
      module Control.Monad.IO.Class
    , module Test.QuickCheck

      -- * Running tests
    , TestEnvironment(..)
    , execTest
    , testRunner

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
import Control.Concurrent.Chan
import Control.Exception (catchJust, finally)
import Control.Monad (forM_, void)
import Control.Monad.CatchIO (Exception, MonadCatchIO, tryJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Configurator
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (SqlError(..))
import System.Environment (getArgs)
import System.IO.Error
import Test.Framework (TestName, buildTest, interpretArgsOrExit, defaultMainWithOpts, ropt_threads)
import Test.QuickCheck hiding (Testable)

import qualified Test.HUnit
import qualified Test.Framework
import qualified Test.Framework.Providers.HUnit
import qualified Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck

import MusicBrainz
import MusicBrainz.Data.Edit

data TestEnvironment = TestEnvironment { testContexts :: Chan Context }

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
  ctxChan <- asks testContexts
  return $
    Test.Framework.Providers.HUnit.testCase testName $ do
      ctx <- readChan ctxChan
      void $ go ctx (3 :: Int) `finally` writeChan ctxChan ctx
  where
    runTest ctx = runMbContext ctx (withTransactionRollBack test)

    isDeadlock (SqlError "40P01" _ _) = Just ()
    isDeadlock _ = Nothing

    go ctx retriesRemaining
      | retriesRemaining <= 0 = runTest ctx
      | otherwise = catchJust isDeadlock (runTest ctx)
                      (const $ go ctx (pred retriesRemaining))

testProperty :: Test.QuickCheck.Testable a => TestName -> a -> Test
testProperty name p = return $ Test.Framework.Providers.QuickCheck2.testProperty name p

(@?=) :: (Eq a, MonadIO m, Show a) => a -> a -> m ()
a @?= b = liftIO (a Test.HUnit.@?= b)

(@?) :: (MonadIO m, Test.HUnit.AssertionPredicable t) => t -> String -> m ()
a @? msg = liftIO (a Test.HUnit.@? msg)

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool message predicate = liftIO (Test.HUnit.assertBool message predicate)

testRunner :: [Test] -> IO ()
testRunner tests = do
    args <- getArgs >>= interpretArgsOrExit
    defaultMainWithOpts
      [buildTest (runner (ropt_threads args) (testGroup "All tests" tests))]
      args
  where
    runner contexts t = do
      testConfig <- load [ Required "test.cfg" ] `catchIOError`
        (\e -> if isDoesNotExistError e
                 then error "test.cfg not found. Please add a test.cfg file, see test.cfg.example for more information."
                 else ioError e)

      let opt key def = lookupDefault (def defaultConnectInfo) testConfig key
      conn <- ConnectInfo
                <$> opt "host" connectHost
                <*> opt "port" connectPort
                <*> opt "user" connectUser
                <*> opt "password" connectPassword
                <*> opt "database" connectDatabase

      ctxChan <- newChan
      forM_ [0..fromMaybe 0 contexts] $
        const (openContext conn >>= writeChan ctxChan)

      ctx <- readChan ctxChan
      runMbContext ctx cleanState
      writeChan ctxChan ctx

      execTest t (TestEnvironment ctxChan)

    cleanState = forM_
      [ "SET client_min_messages TO warning"
      , "TRUNCATE artist_type CASCADE"
      , "TRUNCATE country CASCADE"
      , "TRUNCATE editor CASCADE"
      , "TRUNCATE gender CASCADE"
      , "TRUNCATE label_type CASCADE"
      , "TRUNCATE language CASCADE"
      , "TRUNCATE link_type CASCADE"
      , "TRUNCATE medium_format CASCADE"
      , "TRUNCATE release_group_primary_type CASCADE"
      , "TRUNCATE release_group_secondary_type CASCADE"
      , "TRUNCATE release_status CASCADE"
      , "TRUNCATE script CASCADE"
      , "TRUNCATE track CASCADE"
      , "TRUNCATE work_type CASCADE"
      , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
      ] $ \q -> execute q ()
