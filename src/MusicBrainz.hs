{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| This module provides the 'MusicBrainz' monad for interacting with various
MusicBrainz services, along with convenience re-exports of 'MusicBrainz.Types'
and other very commonly used types, functions and values. -}
module MusicBrainz
    ( -- * The MusicBrainz monad
      MusicBrainz
    , runMb
    , runMbContext
    , openContext

      -- ** Context available in the MusicBrainz monad
    , Context
    , mbDb


      -- * Convenience database functions
    , defaultConnectInfo, connectDatabase, connectUser, connectPassword
    , query, query_, execute, returning, executeMany
    , selectValue
    , withTransaction
    , begin, commit, rollback

      -- * Re-exported modules
    , module MusicBrainz.Types
    ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, Only(..), Query, connect, defaultConnectInfo, connectDatabase, connectUser, connectPassword)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import MusicBrainz.Schema ()
import MusicBrainz.Types

import qualified Database.PostgreSQL.Simple as PG

--------------------------------------------------------------------------------
{-| Context available when executing 'MusicBrainz' actions. -}
data Context = Context
    { mbDb :: Connection
      -- ^ The underlying PostgreSQL 'Connection'.
    }


{-| The MusicBrainz monad allows you to run queries against the MusicBrainz
postgresql database, along with performing abritrary IO. -}
newtype MusicBrainz a = MusicBrainz (ReaderT Context IO a)
  deriving (Monad, Functor, Applicative, MonadReader Context, MonadIO, MonadCatchIO)


{-| Execute MusicBrainz actions in the IO monad. This will open a connection
to the MusicBrainz database, and can be quite expensive if the target doesn't
have pgBouncer running. If this is the case, you should try and call this as
/late/ as possible. -}
runMb :: ConnectInfo -> MusicBrainz a -> IO a
runMb connArgs actions = do
  context <- openContext connArgs
  runMbContext context actions


{-| Run a MusicBrainz action inside a pre-existing context. -}
runMbContext :: Context -> MusicBrainz a -> IO a
runMbContext context  (MusicBrainz actions) = runReaderT actions context


{-| Open a fresh set of connections to interact with MusicBrainz. -}
openContext :: ConnectInfo -> IO Context
openContext connArgs = Context <$> connect connArgs


--------------------------------------------------------------------------------
{-| Run a query, using the active database connection. -}
query :: (FromRow a, ToRow p) => Query -> p -> MusicBrainz [a]
query sql params = withMBConn $ \conn -> PG.query conn sql params


{-| Run a query that takes no parameters, using the active database
connection. -}
query_ :: FromRow a => Query -> MusicBrainz [a]
query_ sql = withMBConn $ \conn -> PG.query_ conn sql


{-| Run a query that returns no data, using the active database connection. -}
execute :: ToRow p => Query -> p -> MusicBrainz Int64
execute sql params = withMBConn $ \conn -> PG.execute conn sql params


{-| Run a query over multiple rows, returning results, using the active
database connection. -}
returning :: (ToRow p, FromRow r) => Query -> [p] -> MusicBrainz [r]
returning sql params = withMBConn $ \conn -> PG.returning conn sql params


{-| Run a query over multiple rows, returning a row count, using the active
database connection. -}
executeMany :: ToRow p => Query -> [p] -> MusicBrainz Int64
executeMany sql params = withMBConn $ \conn -> PG.executeMany conn sql params


{-| Transform query results that only a single row with a single column. -}
selectValue :: Functor m => m [Only a] -> m a
selectValue = fmap (fromOnly . head)


{-| Run a series of MusicBrainz actions within a single PostgreSQL
transaction. -}
withTransaction :: MusicBrainz a -> MusicBrainz a
withTransaction action = begin *> action `onException` rollback <* commit

{-| Begin a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
begin :: MusicBrainz ()
begin = withMBConn (PG.beginMode PG.defaultTransactionMode)

{-| Commit a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
commit :: MusicBrainz ()
commit = withMBConn PG.commit

{-| Rollback a transaction. This is a low-level operation, and generally *not*
what you are really looking for, which is 'withTransaction'. -}
rollback :: MusicBrainz ()
rollback = withMBConn PG.rollback

withMBConn :: (Connection -> IO a) -> MusicBrainz a
withMBConn action = do
  (Context conn) <- ask
  liftIO $ action conn
