{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Provides the 'Create' type class for creating new entities. -}
module MusicBrainz.Class.Create
    ( Create(..) ) where

import Database.PostgreSQL.Simple.FromField (FromField)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.MasterRevision
import MusicBrainz.Class.NewEntityRevision
import MusicBrainz.Class.RealiseTree
import MusicBrainz.Edit
import MusicBrainz.Editor (Editor)
import MusicBrainz.Ref (Ref)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Revision.Internal (newUnlinkedRevision)
import MusicBrainz.Tree (Tree)

--------------------------------------------------------------------------------
{-| The create type class allows you to create new entities. -}
class Create a where
  {-| Create a new entity, with some starting data, producing a fresh MBID. -}
  create :: Ref Editor -> Tree a -> EditT (Ref (Revision a))

  default create
    :: ( Editable a, FromField (Ref a), MasterRevision a, NewEntityRevision a
       , RealiseTree a, RootTable a
       )
    => Ref Editor -> Tree a -> EditT (Ref (Revision a))
  create editor entity = do
    treeId <- realiseTree entity
    entityId <- reserveEntityTable (untag (rootTable :: Tagged a String))
    revisionId <- newUnlinkedRevision editor
    newEntityRevision revisionId entityId treeId
    setMasterRevision entityId revisionId
    includeRevision revisionId
    return revisionId

   where

    reserveEntityTable table = selectValue $ query_ $
      fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")
