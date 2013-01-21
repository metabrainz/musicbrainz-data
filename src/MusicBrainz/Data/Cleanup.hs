module MusicBrainz.Data.Cleanup
    ( eligibleForCleanup ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Loops (orM)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Revision
import MusicBrainz.Data.Relationship

eligibleForCleanup :: (Functor m, MonadIO m, HoldsRelationships a) =>
  Ref (Revision a) -> MusicBrainzT m Bool
eligibleForCleanup r = orM $
    [ Set.null <$> revisionChildren r
    , Set.null <$> viewRelationships r
    ]

