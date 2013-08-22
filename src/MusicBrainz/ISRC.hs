{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MusicBrainz.ISRC where

import Control.Applicative
import Control.Lens
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Text.Parsec hiding ((<|>), optional)
import Text.Parsec.Text ()
import MusicBrainz.Lens (fieldFromPrism, parsecPrism)
import qualified Data.Text as T

--------------------------------------------------------------------------------
newtype ISRC = ISRC Text
  deriving (Eq, Ord, Show, Typeable)

instance FromField ISRC where
  fromField = fieldFromPrism isrc

instance ToField ISRC where
  toField = toField . view (re isrc)

--------------------------------------------------------------------------------
isrc :: Prism' Text ISRC
isrc = parsecPrism (\(ISRC i) -> i) isrcParser
  where
    isrcParser = do
      ISRC . mconcat . map T.pack <$>
        sequence [ countryCode, count 3 upperNum, count 7 digit ] <* eof
      where
        countryCode = count 2 upper
        upperNum = upper <|> digit
