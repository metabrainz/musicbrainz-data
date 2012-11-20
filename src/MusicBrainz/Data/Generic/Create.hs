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
import MusicBrainz.Types.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Edit

--------------------------------------------------------------------------------
data Specification m a = Specification
    { reserveEntity :: MusicBrainzT m (Ref a)
    }


--------------------------------------------------------------------------------
create :: (Functor m, Monad m, MonadIO m, MasterRevision a, NewEntityRevision a, RealiseTree a)
  => Specification m a -> Ref Editor -> Tree a -> MusicBrainzT m (CoreEntity a)
create Specification{..} editor entity = do
  treeId <- realiseTree entity
  entityId <- reserveEntity
  revisionId <- newUnlinkedRevision editor
  newEntityRevision revisionId entityId treeId
  setMasterRevision entityId revisionId
  return CoreEntity { coreRef = entityId
                    , coreRevision = revisionId
                    , coreData = treeData entity
                    }


--------------------------------------------------------------------------------
reserveEntityTable :: (Functor m, MonadIO m, Typeable a, FromField (Ref a)) => String -> MusicBrainzT m (Ref a)
reserveEntityTable table = selectValue $ query_ $
  fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")
