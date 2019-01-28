{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Console.ArgParser
import           Tora.CLI
import qualified Tora.IO.Tail             as ToraTail

main :: IO ()
main = toraCLI >>= flip runApp dispatch

dispatch :: ToraCLI -> IO ()
dispatch cli@(ToraTail {}) =
  ToraTail.exec (_toraIndex cli) "https://tartarus.infra.xerpa.com.br:9998" (_toraQuery cli)
