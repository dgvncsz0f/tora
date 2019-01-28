{-# LANGUAGE OverloadedStrings #-}

module Tora.IO.Tail where

import           Control.Lens
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
wopts = defaults -- FIXME:defaults & auth ?~ basicAuth "USERNAME" "PASSWORD"

exec :: String -> String -> String -> IO ()
exec strIndex strEndpoint strQuery = do
  let baseQuery = SearchQuery (T.pack strQuery) 500 [("@timestamp", SortAsc), ("_id", SortAsc)] Nothing
  case (,) <$> findTemplate strIndex <*> findSearchPath strIndex of
    Nothing     ->
      print "ERROR: could not find template"
    Just (mkTmpl, path) -> do
      uri <- Endpoint <$> mkURI (T.toStrict $ T.pack $ strEndpoint ++ path)
      tmpl <- mkTmpl <$> getCurrentTimeZone
      wsess <- Ws.newSessionControl Nothing tlsManagerSettings
      stream wsess wopts uri baseQuery ((), \_ -> display tmpl)
      pure ()
