{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , query, query_, execute, returning, executeMany
    , selectValue
    , withTransaction
    , begin, commit, rollback
    ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Class
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), Only(..), Query, connect, defaultConnectInfo)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)

import qualified Database.PostgreSQL.Simple as PG

--------------------------------------------------------------------------------
{-| Context available when executing 'MusicBrainz' actions. -}
data Context = Context
    { mbDb :: Connection
      -- ^ The underlying PostgreSQL 'Connection'.
    }


{-| The MusicBrainz monad allows you to run queries against the MusicBrainz
postgresql database, along with performing abritrary IO. -}
type MusicBrainz = MusicBrainzT IO

newtype MusicBrainzT m a = MusicBrainzT (ReaderT Context m a)
  deriving (Monad, Functor, Applicative, MonadReader Context, MonadIO, MonadCatchIO, MonadTrans)


{-| Execute MusicBrainz actions in the IO monad. This will open a connection
to the MusicBrainz database, and can be quite expensive if the target doesn't
have pgBouncer running. If this is the case, you should try and call this as
/late/ as possible. -}
runMb :: (Functor m, MonadIO m) => ConnectInfo -> MusicBrainzT m a -> m a
runMb connArgs actions = do
  context <- openContext connArgs
  runMbContext context actions


{-| Run a MusicBrainz action inside a pre-existing context. -}
runMbContext :: Context -> MusicBrainzT m a -> m a
runMbContext context (MusicBrainzT actions) = runReaderT actions context


{-| Open a fresh set of connections to interact with MusicBrainz. -}
openContext :: (Functor m, MonadIO m) => ConnectInfo -> m Context
openContext connArgs = Context <$> liftIO (connect connArgs)


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
withTransaction action = begin *> action `onException` rollback <* commit


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
withMBConn action = do
  (Context conn) <- ask
  liftIO $ action conn

