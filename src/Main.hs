{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, ScopedTypeVariables, UnicodeSyntax #-}

module Main where

import Prelude

import Control.Applicative
import Control.Lens
import Data.Aeson.Lens (_Object, _String, key)
import Data.Aeson.Types (Value)
import Data.Foldable (fold)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (mapMaybe, maybe)
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import qualified Network.Wreq as Wreq
import qualified Options.Generic as Optparse

--import Currency (Currency)


coinbaseAPI ∷ String → String
coinbaseAPI c =
  "https://api.coinbase.com/v2/exchange-rates?currency=" <> c


data Input = Input
  { baseCurrency ∷ String
  , pickCurrencies ∷ [Text]
  , separator ∷ Text.Text
  , divider ∷ Text.Text
  } deriving (Optparse.Generic, Show)


instance Optparse.ParseRecord Input


main ∷ IO ()
main = do
  input ∷ Input ← Optparse.getRecord "Crypto & Currency Exchange"
  let wreqOpts = Wreq.defaults & Wreq.header "CB-VERSION" .~ [ "2017-11-17" ]
  r ← Wreq.getWith wreqOpts $ coinbaseAPI (baseCurrency input)
  let
    ratesMap ∷ HashMap Text Value
    ratesMap =
      r ^? Wreq.responseBody . key "data" . key "rates" . _Object & fold

    rateStr ∷ Text → Maybe Text
    rateStr c = do
      v ← HashMap.lookup c ratesMap
      a ← v ^? _String
      return $ c <> divider input <> a

  putStrLn . Text.unpack . Text.intercalate (separator input) . mapMaybe rateStr $ pickCurrencies input

