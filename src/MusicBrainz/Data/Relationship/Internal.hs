module MusicBrainz.Data.Relationship.Internal
    ( HoldsRelationships(..) ) where

import Control.Monad.IO.Class (MonadIO)

import MusicBrainz

--------------------------------------------------------------------------------
{-| This type class is implemented for any entities that may have
'Relationship's. -}
class HoldsRelationships a where
  fetchEndPoints :: (Functor m, MonadIO m)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]
