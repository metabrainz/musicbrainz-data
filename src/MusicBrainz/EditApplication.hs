{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.EditApplication where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz.Monad
import MusicBrainz.Artist
import MusicBrainz.Edit
import MusicBrainz.Label
import MusicBrainz.Recording
import MusicBrainz.Ref
import MusicBrainz.Release
import MusicBrainz.ReleaseGroup
import MusicBrainz.Revision
import MusicBrainz.URL
import MusicBrainz.Work

--------------------------------------------------------------------------------
getChanges
  :: (Functor m, MonadIO m)
  => Ref Edit -> MusicBrainzT m [Change]
getChanges editId = map toChange <$> query [sql|
      SELECT 'artist'::text, revision_id FROM edit_artist WHERE edit_id = ?
      UNION ALL
      SELECT 'label'::text, revision_id FROM edit_label WHERE edit_id = ?
      UNION ALL
      SELECT 'recording'::text, revision_id FROM edit_recording WHERE edit_id = ?
      UNION ALL
      SELECT 'release'::text, revision_id FROM edit_release WHERE edit_id = ?
      UNION ALL
      SELECT 'release_group'::text, revision_id FROM edit_release_group WHERE edit_id = ?
      UNION ALL
      SELECT 'url'::text, revision_id FROM edit_url WHERE edit_id = ?
      UNION ALL
      SELECT 'work'::text, revision_id FROM edit_work WHERE edit_id = ?
    |] (editId, editId, editId, editId, editId, editId, editId)
  where
    toChange :: (String, Int) -> Change
    toChange (kind, revisionId) =
      case kind of
        "artist"        -> Change (revisionId ^. reference :: Ref (Revision Artist))
        "label"         -> Change (revisionId ^. reference :: Ref (Revision Label))
        "recording"     -> Change (revisionId ^. reference :: Ref (Revision Recording))
        "release"       -> Change (revisionId ^. reference :: Ref (Revision Release))
        "release_group" -> Change (revisionId ^. reference :: Ref (Revision ReleaseGroup))
        "url"           -> Change (revisionId ^. reference :: Ref (Revision URL))
        "work"          -> Change (revisionId ^. reference :: Ref (Revision Work))
        _               -> error $ "Attempt to load an edit with revision of unknown kind '" ++ kind ++ "'"

--------------------------------------------------------------------------------
{-| Apply an edit by merging all revisions in the edit upstream. -}
apply :: Ref Edit -> MusicBrainz ()
apply editId = do
  getChanges editId >>= mapM_ mergeUpstream
  closeEdit
  where
    mergeUpstream (Change r) = mergeRevisionUpstream r

    closeEdit = void $ execute
      [sql| UPDATE edit SET status = ? WHERE edit_id = ? |] (Closed, editId)
