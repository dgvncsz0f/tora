{-# LANGUAGE OverloadedStrings #-}

module Tora.IO.Tail
  ( exec ) where

import           Control.Lens
import           Data.Maybe
import qualified Data.Text.Lazy          as T
import           Data.Time.LocalTime
import           Network.HTTP.Client.TLS
import           Network.Wreq
import qualified Network.Wreq.Session    as Ws
import           Text.URI
import           Tora.Input
import           Tora.Output
import           Tora.Templates
import           Tora.Types

wopts :: Options
wopts = defaults & auth ?~ basicAuth "USERNAME" "PASSWORD"

exec :: Config -> String -> String -> String -> IO ()
exec strIndex strEndpoint strQuery = do
  let baseQuery = TailRequest (T.pack strQuery) 500 [("@timestamp", SortAsc), ("_id", SortAsc)] Nothing
  case (,) <$> findTemplate strIndex <*> findSearchPath strIndex of
    Nothing     ->
      putStrLn "ERROR: could not find template"
    Just (mkTmpl, path) -> do
      uri <- Endpoint <$> mkURI (T.toStrict $ T.pack $ strEndpoint ++ path)
      tmpl <- mkTmpl <$> getCurrentTimeZone
      wsess <- Ws.newSessionControl Nothing tlsManagerSettings
      stream wsess wopts uri baseQuery
        ((), \_ -> displayTailResult tmpl . fromMaybe (error "ERROR: could not read document"))
      pure ()
