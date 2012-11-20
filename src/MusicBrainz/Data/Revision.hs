{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Fuctions for manipulating revisions. -}
module MusicBrainz.Data.Revision
    ( mergeBase
    , revisionParents
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Set as Set

import MusicBrainz

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
{-| Find references to the parent revisions of a given revision. -}
revisionParents :: (Functor m, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set (Ref (Revision a)))
revisionParents artistRev =
  Set.fromList . map fromOnly <$> query q (Only artistRev)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]
