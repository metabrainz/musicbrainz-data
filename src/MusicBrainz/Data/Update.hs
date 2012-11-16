module MusicBrainz.Data.Update
    ( Update(..)
    ) where

import MusicBrainz
import MusicBrainz.Edit

--------------------------------------------------------------------------------
{-| This type class allows one version of an entity to be replaced
(\'updated\') with another version. -}
class Update a where
  {-| Create a new version of an entity, linked to an existing 'Revision'
  (the parent) and use a new 'Tree'. -}
  update :: Ref Editor -> Ref (Revision a) -> Tree a
    -> EditM (Ref (Revision a))
