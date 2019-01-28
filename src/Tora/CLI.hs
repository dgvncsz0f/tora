module Tora.CLI where

import           System.Console.ArgParser

data ToraCLI
  = ToraTail { _toraQuery    :: String
             , _toraIndex    :: String
             }

parseToraTail :: ParserSpec ToraCLI
parseToraTail =
  ToraTail
  `parsedBy` reqFlag "query"
  `andBy` reqFlag "index"

toraCLI :: IO (CmdLnInterface ToraCLI)
toraCLI = mkSubParser
  [ ("tail", mkDefaultApp parseToraTail "tail") ]
