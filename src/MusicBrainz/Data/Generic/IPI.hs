{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Generic.IPI where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.String
import Database.PostgreSQL.Simple (Only(..))

import qualified Data.Set as Set

import MusicBrainz

--------------------------------------------------------------------------------
viewIpiCodes :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m (Set.Set IPI)
viewIpiCodes entityName r = Set.fromList <$> query q (Only r)
  where q = fromString $ unlines
            [ "SELECT ipi "
            , "FROM " ++ entityName ++ "_ipi "
            , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
            , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
            , "WHERE revision_id = ?"
            ]
