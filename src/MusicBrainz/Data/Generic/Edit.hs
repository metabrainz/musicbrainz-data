{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Generic.Edit
    ( linkRevisionToEdit ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)

import MusicBrainz

--------------------------------------------------------------------------------
linkRevisionToEdit :: (Functor m, MonadIO m)
  => String -> Ref Edit -> Ref (Revision a) -> MusicBrainzT m ()
linkRevisionToEdit table editId revisionId = void $
  execute q (editId, revisionId)
  where q = fromString $ unlines
              [ "INSERT INTO " ++ table ++ " (edit_id, revision_id)"
              , " VALUES (?, ?)"
              ]
