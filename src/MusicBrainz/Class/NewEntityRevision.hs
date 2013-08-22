{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Class.NewEntityRevision where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Ref (Ref)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Tree (Tree)

--------------------------------------------------------------------------------
class NewEntityRevision a where
  newEntityRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()

  default newEntityRevision
    :: (Functor m, MonadIO m, RootTable a
       , ToField (Ref (Tree a)), ToField (Ref (Revision a)), ToField (Ref a))
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()
  newEntityRevision revisionId entityId entityTreeId = void $
    execute q (entityId, revisionId, entityTreeId)
   where
    entityName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
          [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
          , "VALUES (?, ?, ?)"
          ]
