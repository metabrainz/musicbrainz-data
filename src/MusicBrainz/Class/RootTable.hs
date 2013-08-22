module MusicBrainz.Class.RootTable (RootTable(..)) where

import Data.Tagged (Tagged)

--------------------------------------------------------------------------------
class RootTable a where
  rootTable :: Tagged a String
