{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Prelude

import           Control.Applicative
import           Control.Lens
import           Data.Aeson.Lens          (key, _String)
import qualified Data.List                as List
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text
import qualified Data.Text.Format         as Format
import qualified Data.Text.Format.Params  as Format
import qualified Network.Wreq             as Wreq
import qualified Options.Applicative      as Optparse
import qualified Options.Applicative.Text as Optparse


main ∷ IO ()
main = do
  let parserOpts = Optparse.info (currencyParser <**> Optparse.helper) Optparse.fullDesc
  ( crypto, currency ) ← Optparse.execParser parserOpts
  let wreqOpts = Wreq.defaults & Wreq.header "CB-VERSION" .~ [ "2018-12-26" ]
  r ← Wreq.getWith wreqOpts $ coinbaseAPI crypto
  let amount = r ^? Wreq.responseBody . key "data" . key "rates" . key currency . _String
  putStrLn . Text.unpack $ fromMaybe ("Error: `" <> currency <> "` not found.") amount


data Crypto
  = BTC
  | ETH
  | LTC
  | ZEC
  deriving (Bounded, Enum, Eq, Read, Show)


type CurrencyPair =
  ( Crypto, Text.Text )


currencyParser ∷ Optparse.Parser CurrencyPair
currencyParser =
  (,)
    <$> Optparse.option
      Optparse.auto
      (Optparse.long "crypto"
        <> Optparse.metavar "CRYPTO"
        <> Optparse.help ("Cryptocurrency: " <> (List.intercalate " | " $ show <$> [ (minBound :: Crypto) .. ])))
    <*> Optparse.textOption
      (Optparse.long "currency"
        <> Optparse.metavar "CURRENCY"
        <> Optparse.help "Currency: USD | ...")


coinbaseAPI ∷ Crypto → String
coinbaseAPI cry =
  "https://api.coinbase.com/v2/exchange-rates?currency=" <> show cry
