module MusicBrainz.Class.ViewRevision where

import Control.Monad.IO.Class (MonadIO)
import MusicBrainz.Monad
import MusicBrainz.Entity (CoreEntity)
import MusicBrainz.Ref (Ref)
import MusicBrainz.Revision (Revision)

--------------------------------------------------------------------------------
{-| View a specific revision, along with the basic 'treeData'. -}
class ViewRevision a where
  viewRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (CoreEntity a)
