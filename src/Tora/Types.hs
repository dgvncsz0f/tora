{-# LANGUAGE OverloadedStrings #-}

module Tora.Types where

import           Data.Aeson     ((.:), (.:?), (.=))
import qualified Data.Aeson     as A
import qualified Data.Text.Lazy as T
import           Data.Time
import qualified Text.URI       as URI

type Timestamp = UTCTime

newtype Endpoint
  = Endpoint { _uri :: URI.URI }
  deriving (Show)

newtype Cursor
  = Cursor A.Value
  deriving (Eq, Show)

data SortDir
  = SortAsc | SortDesc
  deriving (Show)

data SearchQuery
  = SearchQuery { _sqQuery  :: T.Text
                , _sqLimit  :: Int
                , _sqSort   :: [(T.Text, SortDir)]
                , _sqCursor :: Maybe Cursor
                }
  deriving (Show)

data Severity
  = Debug
  | Info
  | Warn
  | Notice
  | Error
  | Other T.Text
  deriving (Show)

data SearchResult
  = SearchResult { _prTotal  :: Int
                 , _prHits   :: [A.Value]
                 , _prCursor :: Maybe Cursor
                 }
  deriving (Show)

instance A.FromJSON SearchResult where
  parseJSON =
    A.withObject "SearchResult" $ \pres -> do
      hits <- pres .: "hits"
      docs <- hits .: "hits"
      SearchResult
        <$> (hits .: "total")
        <*> pure docs
        <*> if null docs
            then pure Nothing
            else A.withObject "Sort" (.:? "sort") (last docs)

instance A.FromJSON Cursor where
  parseJSON = pure . Cursor

instance A.FromJSON Severity where
  parseJSON =
    A.withText "Severity" $ \severity ->
      case severity of
        "info"    -> pure Info
        "debug"   -> pure Debug
        "warning" -> pure Warn
        "err"     -> pure Error
        "notice"  -> pure Notice
        other     -> pure (Other $ T.fromStrict other)

instance A.ToJSON SortDir where
  toJSON SortAsc  = A.String "asc"
  toJSON SortDesc = A.String "desc"

instance A.ToJSON Cursor where
  toJSON (Cursor json) = A.toJSON json

instance A.ToJSON SearchQuery where
  toJSON sq =
    let mkPairs = \after -> "search_after" .= after : pairs
        pairs   = [ "size"  .= _sqLimit sq
                  , "sort"  .= map (\(k, v) -> A.object [T.toStrict k .= v]) (_sqSort sq)
                  , "query" .= A.object [ "query_string" .= A.object [  "query" .= _sqQuery sq ] ]
                  ]
    in maybe (A.object pairs) (A.object . mkPairs) (_sqCursor sq)
