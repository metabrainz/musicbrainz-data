{-# LANGUAGE FlexibleContexts #-}
module MusicBrainz.Data.Generic.Revision
    ( setMasterRevision ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import Database.PostgreSQL.Simple.ToField (ToField)

import MusicBrainz

--------------------------------------------------------------------------------
setMasterRevision :: (Functor m, MonadIO m, ToField (Ref a))
  => String -> Ref a -> Ref (Revision a) -> MusicBrainzT m ()
setMasterRevision table entityId revisionId = void $
  execute q (revisionId, entityId)
  where
    q = fromString $ unlines
      [ "UPDATE " ++ table ++ " SET master_revision_id = ? "
      , "WHERE " ++ table ++ "_id = ?"
      ]
