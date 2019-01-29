{-# LANGUAGE OverloadedStrings #-}

module Tora.Input
  ( ReduceResult (..)
  , stream
  , fetch
  ) where

import           Control.Concurrent
import           Control.Lens
import           Data.Aeson           hiding (Options)
import           Network.Wreq
import qualified Network.Wreq.Session as WSESS
import qualified Text.URI             as URI
import           Tora.Types

data ReduceResult a = Cont a (Maybe Cursor) | Halt a

reduceWithDelay :: (st -> Maybe TailResult -> IO (ReduceResult st))
                -> (Int, st)
                -> Maybe TailResult
                -> IO (ReduceResult (Int, st))
reduceWithDelay reducer (n, st) results = do
  ans <- reducer st results
  case ans of
    Halt st -> pure $ Halt (n, st)
    Cont st Nothing -> do
      threadDelay (2 ^ ((50 + n) `div` 5) * 1000)
      pure $ Cont (min 30 $ succ n, st) Nothing
    Cont st c@(Just {}) -> pure $ Cont (0, st) c

stream :: WSESS.Session
       -> Options
       -> Endpoint
       -> TailRequest
       -> (st, st -> Maybe TailResult -> IO (ReduceResult st))
       -> IO (ReduceResult st)
stream sess opts endpoint payload (st, reducer) =
  stream' sess opts endpoint payload (0, st) reducer
  where
    stream' sess opts endpoint payload st reducer = do
      rsp <- WSESS.customPayloadMethodWith "GET" opts sess (URI.renderStr (_uri endpoint)) payload
      res <- reduceWithDelay reducer st (decode (rsp ^. responseBody))
      case res of
        Halt (_, st')    -> pure $ Halt st'
        Cont st' Nothing -> stream' sess opts endpoint payload st' reducer
        Cont st' cursor  -> stream' sess opts endpoint (payload {treqCursor = cursor}) st' reducer

fetch :: FromJSON r
      => WSESS.Session
      -> Options
      -> Endpoint
      -> IO (Maybe r)
fetch sess opts endpoint = do
  rsp <- WSESS.getWith opts sess (URI.renderStr (_uri endpoint))
  pure $ decode (rsp ^. responseBody)
