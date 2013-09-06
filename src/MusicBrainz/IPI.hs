{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.IPI where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Data.Typeable
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
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
{-| An \'Interested Parties Information Code\' that can be attached to various
entities. -}
newtype IPI = IPI Text
  deriving (Eq, Ord, Show, Typeable)

instance FromField IPI where
  fromField = fieldFromPrism ipi

instance FromRow IPI where
  fromRow = field

instance ToRow IPI where
  toRow = pure . toField . view (re ipi)


--------------------------------------------------------------------------------
ipi :: Prism' Text IPI
ipi = parsecPrism (\(IPI i) -> i) ipiParser
  where
    ipiParser = IPI . T.pack <$> (try parseIpi <|> parseCae)
      where
        parseCae = ("00" ++) <$> count 9 digit <* eof
        parseIpi = count 11 digit <* eof


--------------------------------------------------------------------------------
{-| Implemented by types that can have IPI codes in their tree, and provides
methods to work with those IPI codes. -}
class ViewIPICodes a where
  {-| Fetch all IPI codes for a set of revisions. -}
  viewIpiCodes :: (Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set IPI))

  default viewIpiCodes
    :: (RootTable a, Functor m, MonadIO m)
    => Set.Set (Ref (Revision a))
    -> MusicBrainzT m (Map.Map (Ref (Revision a)) (Set.Set IPI))
  viewIpiCodes rs = groupMap partitionIpis rs' <$> query q (Only . In $ rs')
   where
    entityName = untag (rootTable :: Tagged a String)
    rs' = Set.toList rs
    partitionIpis (revisionId, ipiCode) = (revisionId, Set.singleton ipiCode)
    q = fromString $ unlines
          [ "SELECT revision_id, ipi "
          , "FROM " ++ entityName ++ "_ipi "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]


--------------------------------------------------------------------------------
{-| Provide a single lens to view the IPI codes inside a 'Tree'. -}
class TreeIPICodes a where
  {-| A 'Lens' into the annotation for any 'Tree'. -}
  ipiCodes :: Lens' (Tree a) (Set IPI)
