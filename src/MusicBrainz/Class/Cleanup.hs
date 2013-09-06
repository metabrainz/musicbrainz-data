module MusicBrainz.Class.Cleanup
    ( eligibleForCleanup ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Loops (orM)

import qualified Data.Set as Set

import MusicBrainz.Monad
import MusicBrainz.Relationship.Internal
import MusicBrainz.Versioning

eligibleForCleanup :: (Functor m, MonadIO m, HoldsRelationships a) =>
  Ref (Revision a) -> MusicBrainzT m Bool
eligibleForCleanup r = fmap not $ orM $
    [ not . Set.null <$> revisionChildren r
    , not . Set.null <$> viewRelationships r
    ]

