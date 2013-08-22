module MusicBrainz.PartialDate
    ( PartialDate
    , dateYear, dateMonth, dateDay
    , emptyDate
    , isEmpty
    , partialDate
    ) where

import Control.Applicative
import Control.Lens
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Data.Ix (inRange)

--------------------------------------------------------------------------------
{-| A partial date consisting of an optional year, month and day.-}
data PartialDate = PartialDate
    { dateYear :: !(Maybe Int)
    , dateMonth :: !(Maybe Int)
    , dateDay :: !(Maybe Int)
    }
  deriving (Eq, Ord, Show)

instance FromRow PartialDate where
  fromRow = PartialDate <$> field <*> field <*> field

instance ToRow PartialDate where
  toRow (PartialDate y m d) = map toField [y, m, d]


--------------------------------------------------------------------------------
{-| A 'PartialDate' with no year, month or day. -}
emptyDate :: PartialDate
emptyDate = PartialDate Nothing Nothing Nothing


--------------------------------------------------------------------------------
{-| Determinate if a 'PartialDate' is the empty date (with no fields set). -}
isEmpty :: PartialDate -> Bool
isEmpty = (== emptyDate)


--------------------------------------------------------------------------------
{-| View a tuple as a partial date, providing it does make some sort of valid
date. -}
partialDate :: Prism' (Maybe Int, Maybe Int, Maybe Int) PartialDate
partialDate = prism dateComponents viewDate
  where
    dateComponents (PartialDate y m d) = (y, m, d)
    viewDate (y, m, d)
      | validMonth m && validDay d = Right (PartialDate y m d)
      | otherwise                  = Left (y, m, d)
    validMonth = maybe True (inRange (1, 12))
    validDay = maybe True (inRange (1, 31))

