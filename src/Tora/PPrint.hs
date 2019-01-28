{-# LANGUAGE OverloadedStrings #-}

module Tora.PPrint
       ( Doc
       , Template ()
       , TemplateSpec (..)
       , Style (..)
       , compile
       , renderInfoResult
       , renderInfoResultNoFields
       , defaultOpts
       , fetchData
       , fetchSeverity
       , fetchTimestamp
       , (<$$>)
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

data Style
  = Fill Int
  | Nest Int
  | BreakLine

type Template = Reader A.Value Doc

data TemplateSpec =
  Data ReadData TemplateSpec
  | StyleT Style TemplateSpec
  | Union TemplateSpec TemplateSpec
  | Done

severity :: Severity -> Doc
severity Debug       = text "DEBUG"
severity Info        = text "INFO"
severity Warn        = text "WARN"
severity Error       = text "ERROR"
severity Notice      = text "NOTICE"
severity (Other sev) = text sev

timestamp :: FormatTime t => TimeLocale -> t -> Doc
timestamp locale = text . TL.pack . formatTime locale "%Y-%m-%d %H:%M:%S%Q%z"

fetchData :: [T.Text] -> A.Value -> Doc
fetchData [] (A.String value) =
  vcat . map text . TL.split (== '\n') . TL.fromStrict $ value
fetchData [] (A.Number value) = text (TL.pack $ show value)
fetchData [] (A.Bool value) = text (TL.pack $ show value)
fetchData (x : xs) (A.Object value) =
  maybe empty (fetchData xs) $ parseMaybe (.: x) value
fetchData _ _ = empty

fetchSeverity :: [T.Text] -> A.Value -> Doc
fetchSeverity [] value =
  maybe empty severity $ parseMaybe parseJSON value
fetchSeverity (x : xs) (A.Object value) =
  maybe empty (fetchSeverity xs) $ parseMaybe (.: x) value
fetchSeverity _ _ = empty

fetchTimestamp :: TimeZone -> [T.Text] -> A.Value -> Doc
fetchTimestamp timezone [] value =
  maybe empty (timestamp defaultTimeLocale . utcToZonedTime timezone)
  $ parseMaybe parseJSON value
fetchTimestamp timezone (x : xs) (A.Object value) =
  maybe empty (fetchTimestamp timezone xs)
  $ parseMaybe (.: x) value
fetchTimestamp _ _ _ = empty

applyStyle :: Style -> Doc -> Doc
applyStyle (Fill n) doc  = fill n doc
applyStyle (Nest n) doc  = nest n doc
applyStyle BreakLine doc = softbreak <> doc

compile :: Opts -> TemplateSpec -> Template
compile opts Done = pure (_optAcc opts)
compile opts (StyleT style tmpl) = do
  acc' <- applyStyle style <$> compile (opts {_optAcc = empty}) tmpl
  pure $ _optJoin opts (_optAcc opts) acc'
compile opts (Data fetch tmpl) = do
  acc' <- _optJoin opts (_optAcc opts) <$> asks fetch
  compile (opts {_optAcc = acc'}) tmpl
compile opts (Union l r) = do
  acc' <- compile opts l
  compile (opts {_optAcc = acc'}) r

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

infixr 0 <$$>
(<$$>) :: TemplateSpec -> TemplateSpec -> TemplateSpec
(<$$>) = Union
