{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Revision where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))

import MusicBrainz.Monad
import MusicBrainz.Class.GetEntity
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)

import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| A revision is a version of an entity at a specific point in time. The type
@a@ indicates what type of entity this is a revision of (e.g., @Revision Artist@
means a specific revision of an 'Artist'). -}
data Revision a = Revision { revisionCreatedAt :: !UTCTime }
  deriving (Eq, Show)

instance Referenceable (Revision a) where
  type RefSpec (Revision a) = Int

instance FromField (Ref (Revision a)) where
  fromField f v = view reference <$> fromField f v

instance FromRow (Revision a) where
  fromRow = Revision <$> field

instance ToField (Ref (Revision a)) where
  toField = toField . dereference


--------------------------------------------------------------------------------
{-| Attempt to resolve a the revision which 2 revisions forked from. -}
mergeBase :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a)
          -> MusicBrainzT m (Maybe (Ref (Revision a)))
mergeBase a b = listToMaybe . map fromOnly <$> query
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
revisionParents r =
  Set.fromList . map fromOnly <$> query q (Only r)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]


--------------------------------------------------------------------------------
revisionChildren :: (Functor m, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set (Ref (Revision a)))
revisionChildren r =
  Set.fromList . map fromOnly <$> query q (Only r)
  where q = [sql| SELECT revision_id FROM revision_parent
                  WHERE parent_revision_id = ? |]


--------------------------------------------------------------------------------
instance GetEntity (Revision a) where
  getEntity r = head <$>
    query [sql| SELECT revision_id, created_at
                FROM revision
                WHERE revision_id = ? |] (Only r)
