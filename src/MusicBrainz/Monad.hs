{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-| This module provides the 'MusicBrainz' monad for interacting with various
MusicBrainz services, along with convenience re-exports of 'MusicBrainz.Types'
and other very commonly used types, functions and values. -}
module MusicBrainz.Monad
    ( -- * The MusicBrainz monad
      MusicBrainz
    , MusicBrainzT
    , runMb
    , runMbContext
    , openContext
    , nestMb

    -- ** Context available in the MusicBrainz monad
    , Context
    , mbDb

      -- * Convenience database functions
    , defaultConnectInfo, ConnectInfo(..)
    , query, query_, execute, execute_, returning, executeMany
    , selectValue
    , begin, commit, rollback
    , withTransaction, withTransactionRollBack
    ) where

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad.CatchIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Trans.Class
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), Only(..), Query, connect, defaultConnectInfo)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Numeric.Natural (Natural)

import qualified Database.PostgreSQL.Simple as PG

--------------------------------------------------------------------------------
{-| Context available when executing 'MusicBrainz' actions. -}
data Context = Context
    { _mbDb :: Connection
      -- ^ The underlying PostgreSQL 'Connection'.
    , _transactionDepth :: Natural
    }

makeLenses ''Context



{-| The MusicBrainz monad allows you to run queries against the MusicBrainz
postgresql database, along with performing abritrary IO. -}
type MusicBrainz = MusicBrainzT IO


{-| The MusicBrainzT monad transformer is the \'bulk\' of the MusicBrainz monad,
and this transformer allows you to use almost any monad as a base monad to
add extra functionality. -}
newtype MusicBrainzT m a = MusicBrainzT (ReaderT Context m a)
  deriving ( Monad, Functor, Applicative, MonadReader Context, MonadIO
           , MonadCatchIO, MonadTrans )


{-| Execute MusicBrainz actions in the IO monad. This will open a connection
to the MusicBrainz database, and can be quite expensive if the target doesn't
have pgBouncer running. If this is the case, you should try and call this as
/late/ as possible. -}
runMb :: (Applicative m, Functor m, MonadIO m) => ConnectInfo -> MusicBrainzT m a -> m a
runMb connArgs actions = do
  context <- openContext connArgs
  runMbContext context actions


{-| Run a MusicBrainz action inside a pre-existing context. -}
runMbContext :: Context -> MusicBrainzT m a -> m a
runMbContext context (MusicBrainzT actions) = runReaderT actions context


{-| Open a fresh set of connections to interact with MusicBrainz. -}
openContext :: (Applicative m, Functor m, MonadIO m) => ConnectInfo -> m Context
openContext connArgs = Context <$> liftIO (connect connArgs)
                               <*> pure 0


{-| Nest a set of 'MusicBrainzT' actions inside another monad, allowing for the
monad to change. -}
nestMb :: Monad m => MusicBrainzT n a -> MusicBrainzT m (n a)
nestMb (MusicBrainzT action) = do
  ctx <- MusicBrainzT ask
  return (runReaderT action ctx)


--------------------------------------------------------------------------------
{-| Run a query, using the active database connection. -}
query :: (MonadIO m, FromRow a, ToRow p) => Query -> p -> MusicBrainzT m [a]
query sql params = withMBConn $ \conn -> PG.query conn sql params


{-| Run a query that takes no parameters, using the active database
connection. -}
query_ :: (MonadIO m, FromRow a) => Query -> MusicBrainzT m [a]
query_ sql = withMBConn $ \conn -> PG.query_ conn sql


{-| Run a query that returns no data, using the active database connection. -}
execute :: (MonadIO m, ToRow p) => Query -> p -> MusicBrainzT m Int64
execute sql params = withMBConn $ \conn -> PG.execute conn sql params


{-| Run a query that returns no data and takes no parameters, using the active
database connection. -}
execute_ :: MonadIO m => Query -> MusicBrainzT m Int64
execute_ sql = withMBConn $ \conn -> PG.execute_ conn sql


{-| Run a query over multiple rows, returning results, using the active
database connection. -}
returning :: (MonadIO m, ToRow p, FromRow r) => Query -> [p] -> MusicBrainzT m [r]
returning sql params = withMBConn $ \conn -> PG.returning conn sql params


{-| Run a query over multiple rows, returning a row count, using the active
database connection. -}
executeMany :: (MonadIO m, ToRow p) => Query -> [p] -> MusicBrainzT m Int64
executeMany sql params = withMBConn $ \conn -> PG.executeMany conn sql params


{-| Transform query results that only a single row with a single column. -}
selectValue :: Functor m => m [Only a] -> m a
selectValue = fmap (fromOnly . head)


{-| Run a series of MusicBrainz actions within a single PostgreSQL
transaction. -}
withTransaction :: (MonadIO m, Applicative m, MonadCatchIO m) => MusicBrainzT m a -> MusicBrainzT m a
withTransaction = withTransaction' commit

{-| Run a series of MusicBrainz actions within a single PostgreSQL
transaction. At the end of the action, roll back the transaction.
This is mostly useful for testing things but leaving with a clean
state. -}
withTransactionRollBack :: (MonadIO m, Applicative m, MonadCatchIO m) => MusicBrainzT m a -> MusicBrainzT m a
withTransactionRollBack = withTransaction' rollback

withTransaction' :: (MonadIO m, Applicative m, MonadCatchIO m) => MusicBrainzT m () -> MusicBrainzT m a -> MusicBrainzT m a
withTransaction' conclude action = view transactionDepth >>= runAt
  where
    action' = local (transactionDepth +~ 1) action
    runAt depth
      | depth == 0 = begin *> action' `onException` rollback <* conclude
      | otherwise  = action'

{-| Begin a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
begin :: MonadIO m => MusicBrainzT m ()
begin = withMBConn (PG.beginMode PG.defaultTransactionMode)


{-| Commit a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
commit :: MonadIO m => MusicBrainzT m ()
commit = withMBConn PG.commit


{-| Rollback a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
rollback :: MonadIO m => MusicBrainzT m ()
rollback = withMBConn PG.rollback


withMBConn :: MonadIO m => (Connection -> IO a) -> MusicBrainzT m a
withMBConn action = view mbDb >>= liftIO . action

