{-# LANGUAGE RecordWildCards #-}
module MusicBrainz.Data.Generic.Create
    ( create
    , Specification(..)
    , reserveEntityTable
    ) where

import Data.String (fromString)
import Data.Typeable (Typeable)

import MusicBrainz

import MusicBrainz.Data.Revision (newRevision)

data Specification a = Specification
    { getTree :: Tree a -> MusicBrainz (Ref (Tree a))
    , reserveEntity :: MusicBrainz (MBID a)
    , newEntityRevision :: MBID a -> Ref (Tree a) -> Ref (Revision a) -> MusicBrainz (Ref (Revision a))
    , linkRevision :: MBID a -> Ref (Revision a) -> MusicBrainz ()
    }

create :: Specification a -> Ref Editor -> Tree a -> MusicBrainz (CoreEntity a)
create Specification{..} editor entity = do
  treeId <- getTree entity
  entityId <- reserveEntity
  revisionId <- newRevision editor >>= newEntityRevision entityId treeId
  linkRevision entityId revisionId
  return CoreEntity { coreMbid = entityId
                    , coreRevision = revisionId
                    , coreData = treeData entity
                    }

reserveEntityTable :: Typeable a => String -> MusicBrainz (MBID a)
reserveEntityTable table = selectValue $ query_ $
  fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")
