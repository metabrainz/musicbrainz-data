module MusicBrainz.Data.Generic.Annotation where

import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))

import MusicBrainz

--------------------------------------------------------------------------------
viewAnnotation :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m Text
viewAnnotation entityName r = fromOnly . head <$> query q (Only r)
  where
    q = fromString $ unlines
        [ "SELECT annotation "
        , "FROM " ++ entityName ++ "_tree "
        , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
        , "WHERE revision_id = ?"
        ]
