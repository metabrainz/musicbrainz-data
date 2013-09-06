{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.ISNI where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (mconcat)
import Data.Set (Set)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text ()

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Lens (fieldFromPrism, parsecPrism)
import MusicBrainz.Util (groupMap)
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
{-| An \'International Standard Name Identifier\' that can be attached to various
entities. -}
newtype ISNI = ISNI Text
  deriving (Eq, Ord, Show, Typeable)

instance FromField ISNI where
  fromField = fieldFromPrism isni

instance ToRow ISNI where
  toRow = pure . toField . view (re isni)

--------------------------------------------------------------------------------
isni :: Prism' Text ISNI
isni = parsecPrism (\(ISNI i) -> i) isniParser
  where
    isniParser = ISNI . T.pack . mconcat <$>
      sequence [ count 15 digit
               , return <$> (digit <|> char 'X')
               ]

--------------------------------------------------------------------------------
{-| Implemented by types that can have ISNI codes in their tree, and provides
methods to work with those ISNI codes. -}
class ViewISNICodes a where
  {-| Fetch all ISNI codes for a set of revisions. -}
  viewIsniCodes :: (Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set ISNI))

  default viewIsniCodes
    :: (Functor m, MonadIO m, RootTable a)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set ISNI))
  viewIsniCodes rs =
    groupMap partitionIsnis rs' <$> query q (Only . In $ rs')
   where
    entityName = untag (rootTable :: Tagged a String)
    rs' = Set.toList rs
    partitionIsnis (revisionId, isniCode) = (revisionId, Set.singleton isniCode)
    q = fromString $ unlines
          [ "SELECT revision_id, isni "
          , "FROM " ++ entityName ++ "_isni "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]


--------------------------------------------------------------------------------
{-| Provide a single lens to view the ISNI codes inside a 'Tree'. -}
class TreeISNICodes a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  isniCodes :: Lens' (Tree a) (Set ISNI)
