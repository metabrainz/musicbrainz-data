{-# LANGUAGE DefaultSignatures #-}
module MusicBrainz.Data.Relationship.Internal where

import Control.Monad.IO.Class
import Data.Set as Set
import MusicBrainz
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Edit

class HoldsRelationships a where
  fetchEndPoints :: (Functor m, MonadIO m)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]

  default fetchEndPoints
    :: (Functor m, MonadIO m, CoreEntityTable a)
    => Ref (Revision a) -> RelationshipTarget
    -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]

  fetchEndPoints = undefined

  reflectRelationshipChange ::
    Ref Editor -> Ref a -> (LinkedRelationship -> Set LinkedRelationship -> Set LinkedRelationship) -> LinkedRelationship -> EditM ()

