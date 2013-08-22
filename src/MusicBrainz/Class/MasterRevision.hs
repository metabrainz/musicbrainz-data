{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Class.MasterRevision where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Database.PostgreSQL.Simple.ToField (ToField)

import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Ref (Ref)
import MusicBrainz.Revision (Revision)

--------------------------------------------------------------------------------
class MasterRevision a where
  setMasterRevision :: (Functor m, MonadIO m)
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()

  default setMasterRevision
    :: (Functor m, MonadIO m, RootTable a, ToField (Ref a), ToField (Ref (Revision a)))
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()
  setMasterRevision entityId revisionId = void $
    execute q (revisionId, entityId)
   where
    table = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
      [ "UPDATE " ++ table ++ " SET master_revision_id = ? "
      , "WHERE " ++ table ++ "_id = ?"
      ]
