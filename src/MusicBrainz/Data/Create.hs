{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Provides the 'Create' type class for creating new entities. -}
module MusicBrainz.Data.Create
    ( Create(..) ) where

import Database.PostgreSQL.Simple.FromField (FromField)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import MusicBrainz
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Edit

--------------------------------------------------------------------------------
{-| The create type class allows you to create new entities. -}
class Create a where
  {-| Create a new entity, with some starting data, producing a fresh MBID. -}
  create :: Ref Editor -> Tree a -> EditM (Ref (Revision a))

  default create
    :: ( Editable a, FromField (Ref a), MasterRevision a, NewEntityRevision a
       , RealiseTree a, CoreEntityTable a
       )
    => Ref Editor -> Tree a -> EditM (Ref (Revision a))
  create editor entity = do
    treeId <- realiseTree entity
    entityId <- reserveEntityTable (untag (rootTable :: Tagged a String))
    revisionId <- newUnlinkedRevision editor
    MusicBrainz.Edit.newEntityRevision revisionId entityId treeId
    MusicBrainz.Edit.setMasterRevision entityId revisionId
    includeRevision revisionId
    return revisionId

   where

    reserveEntityTable table = selectValue $ query_ $
      fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")
