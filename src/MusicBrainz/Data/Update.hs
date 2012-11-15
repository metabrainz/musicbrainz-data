module MusicBrainz.Data.Update
    ( Update(..)
    ) where

import MusicBrainz
import MusicBrainz.Edit

class Update a where
  update :: Ref Editor -> Ref (Revision a) -> Tree a
    -> EditM (Ref (Revision a))
