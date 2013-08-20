module MusicBrainz.Data.CoreEntity (CoreEntityTable(..)) where

import Data.Tagged (Tagged)

--------------------------------------------------------------------------------
class CoreEntityTable a where
  rootTable :: Tagged a String
