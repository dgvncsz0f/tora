module Tora.CLI
  ( ToraCLI (..)
  , toraCLI
  ) where

import           System.Console.ArgParser

data ToraCLI
  = ToraTail { _toraQuery :: String
             , _toraIndex :: String
             }
  | ToraInfo { _toraIndex    :: String
             , _toraNoFields :: Bool
             }

parseToraTail :: ParserSpec ToraCLI
parseToraTail =
  ToraTail
  `parsedBy` reqFlag "query"
  `andBy` reqFlag "index"

parseToraInfo :: ParserSpec ToraCLI
parseToraInfo =
  ToraInfo
  `parsedBy` optFlag "logstash-*" "index"
  `andBy` boolFlag "no-fields"

toraCLI :: IO (CmdLnInterface ToraCLI)
toraCLI = mkSubParser
  [ ("tail", mkDefaultApp parseToraTail "tail")
  , ("info", mkDefaultApp parseToraInfo "info")
  ]
