{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Editor
    ( -- * Finding editors
      findEditorByName
    ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.SqlQQ
import MusicBrainz

findEditorByName :: Text -> MusicBrainz (Maybe (Entity Editor))
findEditorByName name =
  listToMaybe <$> query [sql| SELECT editor_id, name FROM editor WHERE name = ? |]
             (Only name)
