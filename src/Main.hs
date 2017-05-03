{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Maybe (maybe)
import Data.Monoid ((<>), mappend)
import qualified Data.Text as Text
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import Prelude

import Data.Aeson.Lens (_String, key)
import qualified Network.Wreq as Wreq
import qualified Options.Applicative as Optparse

main :: IO ()
main = do
  let parserOpts = Optparse.info (currencyParser <**> Optparse.helper) Optparse.fullDesc
  currencyPair <- Optparse.execParser parserOpts
  let wreqOpts = Wreq.defaults & Wreq.header "CB-VERSION" .~ ["2017-03-04"]
  r <- Wreq.getWith wreqOpts $ coinbaseApi currencyPair
  let amount = r ^? Wreq.responseBody . key "data" . key "amount" . _String
  putStrLn . maybe "Err" Text.unpack $ amount

data Crypto
  = BTC
  | ETH
  | LTC
  deriving (Eq, Read, Show)

data CurrencyPair =
  CurrencyPair Crypto
               String

currencyParser :: Optparse.Parser CurrencyPair
currencyParser =
  CurrencyPair <$>
  Optparse.option
    Optparse.auto
    (Optparse.long "crypto" <> Optparse.metavar "CRYPTO" <>
     Optparse.help "Cryptocurrency: BTC | ETH | LTC") <*>
  Optparse.strOption
    (Optparse.long "currency" <> Optparse.metavar "CURRENCY" <> Optparse.help "Currency: USD | ...")

coinbaseApi :: CurrencyPair -> String
coinbaseApi (CurrencyPair cry cur) =
  "https://api.coinbase.com/v2/prices/" <> show cry <> "-" <> cur <> "/spot"
