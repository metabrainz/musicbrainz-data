{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Functions for working with entity annotations. -}
module MusicBrainz.Data.Annotation
    ( ViewAnnotation(..) ) where

import Control.Applicative
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Only(..))
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)

import MusicBrainz
import MusicBrainz.Data.CoreEntity

--------------------------------------------------------------------------------
{-| This type class provides functions for working with annotations for specific
entity types. -}
class ViewAnnotation a where
  {-| Fetch the annotation for a given revision of an entity. -}
  viewAnnotation :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m Text

  default viewAnnotation
    :: (Functor m, MonadIO m, CoreEntityTable a)
    => Ref (Revision a) -> MusicBrainzT m Text
  viewAnnotation r = fromOnly . head <$> query q (Only r)
   where
    entityName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "SELECT annotation "
        , "FROM " ++ entityName ++ "_tree "
        , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
        , "WHERE revision_id = ?"
        ]

