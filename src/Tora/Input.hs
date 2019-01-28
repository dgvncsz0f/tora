{-# LANGUAGE OverloadedStrings #-}

module Tora.Input where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Aeson           hiding (Options)
import           Network.Wreq
import qualified Network.Wreq.Session as WSESS
import qualified Text.URI             as URI
import           Tora.Types

data ReduceResult a = Cont a (Maybe Cursor) | Halt a

reduceWithDelay :: (st -> Maybe SearchResult -> IO (ReduceResult st))
                -> (Int, st)
                -> Maybe SearchResult
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
       -> SearchQuery
       -> (st, st -> Maybe SearchResult -> IO (ReduceResult st))
       -> IO (ReduceResult st)
stream sess opts api query (st, reducer) =
  stream' sess opts api query (0, st) reducer

stream' sess opts api query st reducer = do
  rsp <- WSESS.customPayloadMethodWith "GET" opts sess (URI.renderStr (_uri api)) (toJSON query)
  res <- reduceWithDelay reducer st (decode (rsp ^. responseBody))
  case res of
    Halt (_, st')    -> pure $ Halt st'
    Cont st' Nothing -> stream' sess opts api query st' reducer
    Cont st' cursor  -> stream' sess opts api (query {_sqCursor = cursor}) st' reducer
