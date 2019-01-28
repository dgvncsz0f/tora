{-# LANGUAGE OverloadedStrings #-}

module Tora.Types where

import           Data.Aeson          ((.:), (.:?), (.=))
import qualified Data.Aeson          as A
import           Data.Aeson.Types    (Parser)
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import           Network.Wreq.Types
import qualified Text.URI            as URI

newtype Endpoint
  = Endpoint { _uri :: URI.URI }
  deriving (Show)

newtype Cursor
  = Cursor A.Value
  deriving (Eq, Show)

data SortDir
  = SortAsc | SortDesc
  deriving (Show)

data TailRequest
  = TailRequest { treqQuery  :: TL.Text
                , treqLimit  :: Int
                , treqSort   :: [(TL.Text, SortDir)]
                , treqCursor :: Maybe Cursor
                }
  deriving (Show)

data Severity
  = Debug
  | Info
  | Warn
  | Notice
  | Error
  | Other TL.Text
  deriving (Show)

data TailResult
  = TailResult { trstTotal  :: Int
               , trstHits   :: [A.Value]
               , trstCursor :: Maybe Cursor
               }
  deriving (Show)


type Index = TL.Text

type FieldName = TL.Text

type FieldType = TL.Text

data InfoResult
  = InfoResult { infoResult :: [(Index, [(FieldName, FieldType)])] }
  deriving (Show)

instance A.FromJSON TailResult where
  parseJSON =
    A.withObject "TailResult" $ \res -> do
      hits <- res .: "hits"
      docs <- hits .: "hits"
      TailResult
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
        other     -> pure (Other $ TL.fromStrict other)

instance A.FromJSON InfoResult where
  parseJSON =
    A.withObject "InfoResult" $ parseResult [] . H.toList
    where
      parseFields :: [(FieldName, FieldType)]
                  -> TL.Text
                  -> [(TL.Text, A.Value)]
                  -> Parser [(FieldName, FieldType)]
      parseFields acc _ [] = pure acc
      parseFields acc prefix ((k, A.Object v) : xs) = do
        mtype  <- v .:? "type"
        fields <- maybe [] H.toList <$> (v .:? "properties")
        newAcc <- parseFields acc (prefix <> k <> ".") fields
        case mtype of
          Just (A.String type_) -> do
            parseFields ((prefix <> k, TL.fromStrict type_) : newAcc) prefix xs
          _ ->
            parseFields newAcc prefix xs
      parseFields acc prefix (_ : xs) = parseFields acc prefix xs

      parseResult :: [(Index, [(FieldName, FieldType)])]
                  -> [(T.Text, A.Value)]
                  -> Parser InfoResult
      parseResult acc [] = pure $ InfoResult acc
      parseResult acc ((k, A.Object idx) : xs) = do
        maps   <- idx .: "mappings"
        doc    <- maps .: "doc"
        props  <- doc .: "properties"
        fields <- parseFields [] "" (H.toList props)
        parseResult ((TL.fromStrict k, fields) : acc) xs
      parseResult acc (_ : xs) = parseResult acc xs

instance A.ToJSON SortDir where
  toJSON SortAsc  = A.String "asc"
  toJSON SortDesc = A.String "desc"

instance A.ToJSON Cursor where
  toJSON (Cursor json) = A.toJSON json

instance A.ToJSON TailRequest where
  toJSON sq =
    let mkPairs = \after -> "search_after" .= after : pairs
        pairs   = [ "size"  .= treqLimit sq
                  , "sort"  .= map (\(k, v) -> A.object [TL.toStrict k .= v]) (treqSort sq)
                  , "query" .= A.object [ "query_string" .= A.object [  "query" .= treqQuery sq ] ]
                  ]
    in maybe (A.object pairs) (A.object . mkPairs) (treqCursor sq)

instance Postable TailRequest where
  postPayload q = postPayload (A.toJSON q)
