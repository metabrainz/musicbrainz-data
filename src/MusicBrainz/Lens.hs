{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module MusicBrainz.Lens
    ( fieldFromPrism
    , parsecPrism
    ) where

import Control.Error (hush)
import Control.Lens
import Data.Functor.Identity (Identity)
import Data.Typeable (Typeable)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text ()

import qualified Database.PostgreSQL.Simple.FromField as FF


--------------------------------------------------------------------------------
parsecPrism :: Stream a Identity Char => (c -> a) -> Parsec a () c -> Prism' a c
parsecPrism extract parser = prism' extract (hush . parse parser "")


--------------------------------------------------------------------------------
fieldFromPrism :: (FF.FromField s, Typeable a) => Prism' s a -> FF.FieldParser a
fieldFromPrism p f v = do
  FF.fromField f v >>= maybe conversionFailure return . preview p
 where
  conversionFailure =
    FF.returnError FF.ConversionFailed f "Failed to parse field"
