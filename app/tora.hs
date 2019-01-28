{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Console.ArgParser
import           Tora.CLI
import qualified Tora.IO.Info             as ToraInfo
import qualified Tora.IO.Tail             as ToraTail

main :: IO ()
main = toraCLI >>= flip runApp dispatch

dispatch :: ToraCLI -> IO ()
dispatch argv@(ToraTail {}) =
  ToraTail.exec (_toraIndex argv) "https://tartarus.infra.xerpa.com.br:9998" (_toraQuery argv)
dispatch argv@(ToraInfo {}) =
  ToraInfo.exec (_toraNoFields argv) (_toraIndex argv) "https://tartarus.infra.xerpa.com.br:9998"
