{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Maybe (maybe)
import Data.Monoid ((<>), mappend)
import Data.Text as Text
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

--data Crypto
--  = BTC
--  | ETH
--  deriving (Eq, Read, Show)
currencyParser :: Optparse.Parser (String, String)
currencyParser =
  (,) <$>
  Optparse.strOption
    (Optparse.long "crypto" <> Optparse.metavar "CRYPTO" <>
     Optparse.help "Cryptocurrency: BTC | ETH") <*>
  Optparse.strOption
    (Optparse.long "currency" <> Optparse.metavar "CURRENCY" <> Optparse.help "Currency: USD | ...")

coinbaseApi :: (String, String) -> String
coinbaseApi (cry, cur) = "https://api.coinbase.com/v2/prices/" <> cry <> "-" <> cur <> "/spot"
