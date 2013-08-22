{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Tree where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

import MusicBrainz.Monad
import MusicBrainz.Alias
import MusicBrainz.IPI
import MusicBrainz.ISNI
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Relationship (LinkedRelationship)
import MusicBrainz.Revision (Revision)

--------------------------------------------------------------------------------
{-| Trees for entities are a somewhat internal concept of the way MusicBrainz
versioning works. A tree consists of all the data that is versioned for a
specific entity (of type @a@). -}
class HasTree a where
  data Tree a :: *

  {-| A convenience accessor to the \'essential\' data inside a tree (the data
  which contains the entities, and so on.) -}
  treeData :: Tree a -> a

instance Referenceable (Tree a) where
  type RefSpec (Tree a) = Int

instance FromField (Ref (Tree a)) where
  fromField f v = view reference <$> fromField f v

instance ToField (Ref (Tree a)) where
  toField = toField . dereference

--------------------------------------------------------------------------------
{-| Provide a single lens to view all relationships inside a 'Tree'. -}
class TreeRelationships a where
  {-| A 'Lens' into all relationships for any 'Tree'. -}
  relationships :: Lens' (Tree a) (Set LinkedRelationship)

--------------------------------------------------------------------------------
{-| View all data about a specific version of an entity. -}
class ViewTree a where
  viewTree :: (Applicative m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Tree a)

--------------------------------------------------------------------------------
{-| Provide a single lens to view the IPI codes inside a 'Tree'. -}
class TreeIPICodes a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  ipiCodes :: Lens' (Tree a) (Set IPI)

--------------------------------------------------------------------------------
{-| Provide a single lens to view the ISNI codes inside a 'Tree'. -}
class TreeISNICodes a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  isniCodes :: Lens' (Tree a) (Set ISNI)

--------------------------------------------------------------------------------
{-| Provide a single lens to view the annotation inside a 'Tree'. -}
class TreeAnnotation a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  annotation :: Lens' (Tree a) Text

--------------------------------------------------------------------------------
{-| Provide a single lens to view all aliases inside a 'Tree'. -}
class TreeAliases a where
  {-| A 'Lens' into all aliases for any 'Tree'. -}
  aliases :: Lens' (Tree a) (Set (Alias a))
