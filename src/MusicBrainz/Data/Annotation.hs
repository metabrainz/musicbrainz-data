module MusicBrainz.Data.Annotation
    ( HasAnnotation(..) ) where

import Control.Monad.IO.Class
import Data.Text

import MusicBrainz

class HasAnnotation a where
  viewAnnotation :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m Text
