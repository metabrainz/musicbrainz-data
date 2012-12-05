{-| Provides the 'Create' type class for creating new entities. -}
module MusicBrainz.Data.Create
    ( Create(..) ) where

import MusicBrainz
import MusicBrainz.Edit

--------------------------------------------------------------------------------
{-| The create type class allows you to create new entities. -}
class Create a where
  {-| Create a new entity, with some starting data, producing a fresh MBID. -}
  create :: Ref Editor -> Tree a -> EditM (Ref (Revision a))
