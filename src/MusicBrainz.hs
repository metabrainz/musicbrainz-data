{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MusicBrainz
    ( -- * The MusicBrainz monad
      MusicBrainz
    , runMB

      -- * Convenience database functions
    , query

    , module MusicBrainz.Schema
    , module MusicBrainz.Types
    ) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, Query, connect)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import MusicBrainz.Schema
import MusicBrainz.Types

import qualified Database.PostgreSQL.Simple as PG

--------------------------------------------------------------------------------
{-| Context available when executing 'MusicBrainz' actions. -}
data Context = Context { mbDb :: Connection }


{-| The MusicBrainz monad allows you to run queries against the MusicBrainz
postgresql database, along with performing abritrary IO. -}
newtype MusicBrainz a = MusicBrainz (ReaderT Context IO a)
  deriving (Monad, Functor, Applicative, MonadReader Context, MonadIO)


{-| Execute MusicBrainz actions in the IO monad. -}
runMB :: ConnectInfo -> MusicBrainz a -> IO a
runMB connArgs (MusicBrainz actions) = do
  conn <- connect connArgs
  runReaderT actions (Context conn)


--------------------------------------------------------------------------------
{-| Run a query, using the active database connection. -}
query :: (FromRow a, ToRow p) => Query -> p -> MusicBrainz [a]
query sql params = withMBConn $ \conn -> PG.query conn sql params

withMBConn :: (Connection -> IO a) -> MusicBrainz a
withMBConn action = do
  (Context conn) <- ask
  liftIO $ action conn
