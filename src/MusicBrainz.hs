{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| This module provides the 'MusicBrainz' monad for interacting with various
MusicBrainz services, along with convenience re-exports of 'MusicBrainz.Types'
and other very commonly used types, functions and values. -}
module MusicBrainz
    ( -- * The MusicBrainz monad
      MusicBrainz
    , runMb
      -- ** Context available in the MusicBrainz monad
    , Context
    , mbDb


      -- * Convenience database functions
    , defaultConnectInfo, connectDatabase, connectUser
    , query, query_, execute
    , withTransaction

      -- * Re-exported modules
    , module MusicBrainz.Types
    ) where

import Control.Applicative (Applicative)
import Control.Monad.CatchIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, Query, connect, defaultConnectInfo, connectDatabase, connectUser)
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
runMb connArgs (MusicBrainz actions) = do
  conn <- connect connArgs
  runReaderT actions Context { mbDb = conn }


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


{-| Run a series of MusicBrainz actions within a single PostgreSQL
transaction. -}
withTransaction :: MusicBrainz a -> MusicBrainz a
withTransaction action = do
  withMBConn (PG.beginMode PG.defaultTransactionMode)
  r <- action `onException` (withMBConn PG.rollback)
  withMBConn PG.commit
  return r


withMBConn :: (Connection -> IO a) -> MusicBrainz a
withMBConn action = do
  (Context conn) <- ask
  liftIO $ action conn
