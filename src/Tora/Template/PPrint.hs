{-# LANGUAGE OverloadedStrings #-}

module Tora.Template.PPrint
  ( Doc
  , renderInfoResult
  , renderInfoResultNoFields
  , defaultOpts
  , fetchData
  , fetchTimestamp
  , multiline
  , singleline
  ) where

import           Control.Monad.Reader
import           Data.Aeson                   ((.:))
import qualified Data.Aeson                   as A
import           Data.Aeson.Types             (parseJSON, parseMaybe)
import           Data.List
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.PrettyPrint.Leijen.Text hiding ((<$$>), (<$>))
import           Tora.Types

type ReadData = (A.Value -> Doc)

defaultOpts :: Opts
defaultOpts = Opts (<+>) empty

data Opts
  = Opts { _optJoin :: Doc -> Doc -> Doc
         , _optAcc  :: Doc
         }

timestamp :: FormatTime t => TimeLocale -> t -> Doc
timestamp locale = text . TL.pack . formatTime locale "%Y-%m-%d %H:%M:%S%Q%z"

multiline :: T.Text -> Doc
multiline = vcat . map text . TL.split (== '\n') . TL.fromStrict

singleline :: T.Text -> Doc
singleline = text . TL.fromStrict

fetchData :: [T.Text] -> (T.Text -> Doc) -> A.Value -> Doc
fetchData [] f (A.String value) = f value
fetchData [] _ (A.Number value) = text (TL.pack $ show value)
fetchData [] _ (A.Bool value) = text (TL.pack $ show value)
fetchData (x : xs) f (A.Object value) =
  maybe empty (fetchData xs f) $ parseMaybe (.: x) value
fetchDataML _ _ _ = empty

fetchTimestamp :: TimeZone -> [T.Text] -> A.Value -> Doc
fetchTimestamp timezone [] value =
  maybe empty (timestamp defaultTimeLocale . utcToZonedTime timezone)
  $ parseMaybe parseJSON value
fetchTimestamp timezone (x : xs) (A.Object value) =
  maybe empty (fetchTimestamp timezone xs)
  $ parseMaybe (.: x) value
fetchTimestamp _ _ _ = empty

renderInfoResultNoFields :: InfoResult -> Doc
renderInfoResultNoFields (InfoResult items) = vcat (map (text . fst) $ sort items)

renderInfoResult :: InfoResult -> Doc
renderInfoResult (InfoResult items) = vcat (map dindex $ sort items)
  where
    dindex (index, fields) =
      vcat [ text index
           , indent 2 (vcat (map dfield $ sort fields))
           ]
      <> linebreak

    dfield (name, value) =
      text name <> text ": " <> text value
