{-# LANGUAGE OverloadedStrings #-}

module Tora.IO.Info where

import           Control.Lens
import           Data.Maybe
import qualified Data.Text.Lazy          as T
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

exec :: Bool -> String -> String -> IO ()
exec noFields strIndex strEndpoint = do
  case findMappingPath strIndex of
    Nothing     ->
      print "ERROR: could not find index"
    Just path -> do
      uri <- Endpoint <$> mkURI (T.toStrict $ T.pack $ strEndpoint ++ path)
      sess <- Ws.newSessionControl Nothing tlsManagerSettings
      resp <- fromMaybe (error "ERROR: could not read index") <$> fetch sess wopts uri
      displayInfoResult noFields resp
