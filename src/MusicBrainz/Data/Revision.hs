{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Fuctions for manipulating revisions. -}
module MusicBrainz.Data.Revision
    ( newRevision
    , addChild
    , mergeBase
    , revisionParents
    , ViewRevision(..)
    , CloneRevision(..)
    ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
{-| Create a new \'system\' revision, that is not yet bound to any entity. -}
newRevision :: (Functor m, MonadIO m) => Ref Editor -> MusicBrainzT m (Ref (Revision a))
newRevision editor = selectValue $
  query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
    (Only editor)


--------------------------------------------------------------------------------
{-| Add one 'Revision' as a child of another (parent) 'Revision'. -}
addChild :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a) -> MusicBrainzT m ()
addChild childRevision parentRevision =
  void $ execute
    [sql| INSERT INTO revision_parent (revision_id, parent_revision_id)
          VALUES (?, ?) |] (childRevision, parentRevision)


--------------------------------------------------------------------------------
{-| Attempt to resolve a the revision which 2 revisions forked from. -}
mergeBase :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a)
          -> MusicBrainzT m (Maybe (Ref (Revision a)))
mergeBase a b = selectValue $ query
  [sql| WITH RECURSIVE revision_path (revision_id, parent_revision_id, distance)
        AS (
          SELECT revision_id, parent_revision_id, 1
          FROM revision_parent
          WHERE revision_id IN ?

          UNION

          SELECT
            revision_path.revision_id, revision_parent.parent_revision_id,
            distance + 1
          FROM revision_parent
          JOIN revision_path
            ON (revision_parent.revision_id = revision_path.parent_revision_id)
        )
        SELECT parent_revision_id
        FROM revision_path a
        JOIN revision_path b USING (parent_revision_id)
        ORDER BY a.distance, b.distance
        LIMIT 1 |] (Only $ In [a, b])


--------------------------------------------------------------------------------
class ViewRevision a where
  viewRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (CoreEntity a)


--------------------------------------------------------------------------------
{-| Find references to the parent revisions of a given revision. -}
revisionParents :: (Functor m, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set (Ref (Revision a)))
revisionParents artistRev =
  Set.fromList . map fromOnly <$> query q (Only artistRev)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]


--------------------------------------------------------------------------------
class CloneRevision a where
  cloneRevision :: (Functor m, MonadIO m)
    => CoreEntity a -> Ref Editor -> MusicBrainzT m (Ref (Revision a))
