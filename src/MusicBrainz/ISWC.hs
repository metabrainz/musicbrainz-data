{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MusicBrainz.ISWC where

import Control.Applicative
import Control.Lens
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Text.Parsec hiding ((<|>), optional)
import Text.Parsec.Text ()
import MusicBrainz.Lens (fieldFromPrism, parsecPrism)
import qualified Data.Text as T

--------------------------------------------------------------------------------
newtype ISWC = ISWC Text
  deriving (Eq, Ord, Show, Typeable)

instance FromField ISWC where
  fromField = fieldFromPrism iswc

instance ToField ISWC where
  toField = toField . view (re iswc)


--------------------------------------------------------------------------------
iswc :: Prism' Text ISWC
iswc = parsecPrism (\(ISWC t) -> t) iswcParser
  where
    iswcParser = do
      char 'T' *> dash
      digits <- sequence [ count 3 digit <* dot
                         , count 3 digit <* dot
                         , count 3 digit <* oneOf ".-"
                         ]
      fin <- digit <* eof
      pure $ ISWC $ formatIswc digits fin
      where
        optChar = optional . char
        dash = optChar '-'
        dot  = optChar '.'
        formatIswc digits fin =
          let digits' = T.intercalate "." (map T.pack digits)
          in "T-" <> digits' <> "-" <> T.pack [fin]
