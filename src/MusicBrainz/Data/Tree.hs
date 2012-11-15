module MusicBrainz.Data.Tree
    ( ViewTree(..) ) where

import Control.Applicative
import Control.Monad.IO.Class

import MusicBrainz

--------------------------------------------------------------------------------
{-| View all data about a specific version of an entity. -}
class ViewTree a where
  viewTree :: (Applicative m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Tree a)
