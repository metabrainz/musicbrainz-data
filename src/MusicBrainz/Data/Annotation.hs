{-| Functions for working with entity annotations. -}
module MusicBrainz.Data.Annotation
    ( HasAnnotation(..) ) where

import Control.Monad.IO.Class
import Data.Text

import MusicBrainz

--------------------------------------------------------------------------------
{-| This type class provides functions for working with annotations for specific
entity types. -}
class HasAnnotation a where
  {-| Fetch the annotation for a given revision of an entity. -}
  viewAnnotation :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m Text
