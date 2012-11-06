{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module MusicBrainz.Data.Generic.Create
    ( create
    , Specification(..)
    , reserveEntityTable
    ) where

import Control.Monad.IO.Class
import Data.String (fromString)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField)

import MusicBrainz

import MusicBrainz.Data.Revision (newRevision)

data Specification m a = Specification
    { getTree :: Tree a -> MusicBrainzT m (Ref (Tree a))
    , reserveEntity :: MusicBrainzT m (Ref a)
    , newEntityRevision :: Ref a -> Ref (Tree a) -> Ref (Revision a) -> MusicBrainzT m (Ref (Revision a))
    , linkRevision :: Ref a -> Ref (Revision a) -> MusicBrainzT m ()
    }

create :: (Functor m, Monad m, MonadIO m) => Specification m a -> Ref Editor -> Tree a -> MusicBrainzT m (CoreEntity a)
create Specification{..} editor entity = do
  treeId <- getTree entity
  entityId <- reserveEntity
  revisionId <- newRevision editor >>= newEntityRevision entityId treeId
  linkRevision entityId revisionId
  return CoreEntity { coreRef = entityId
                    , coreRevision = revisionId
                    , coreData = treeData entity
                    }

reserveEntityTable :: (Functor m, MonadIO m, Typeable a, FromField (Ref a)) => String -> MusicBrainzT m (Ref a)
reserveEntityTable table = selectValue $ query_ $
  fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")
