module MusicBrainz.Class.RealiseTree where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz.Monad
import MusicBrainz.Ref (Ref)
import MusicBrainz.Tree (Tree)

--------------------------------------------------------------------------------
class RealiseTree a where
  realiseTree :: (Functor m, MonadIO m)
    => Tree a -> MusicBrainzT m (Ref (Tree a))
