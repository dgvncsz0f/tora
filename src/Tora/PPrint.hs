{-# LANGUAGE OverloadedStrings #-}

module Tora.PPrint
       ( Doc
       , Template ()
       , TemplateSpec (..)
       , Style (..)
       , compile
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

appname :: TL.Text -> TL.Text -> Doc
appname app pid = fill 5 (text app) <> (enclose (text "[") (text "]") (text pid))

hostname :: TL.Text -> Doc
hostname = fill 10 . text

timestamp :: FormatTime t => TimeLocale -> t -> Doc
timestamp locale = text . TL.pack . formatTime locale "%Y-%m-%d %H:%M:%S%Q%z"

logmsg :: TL.Text -> Doc
logmsg = vcat . map text . TL.split (== '\n')

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

fetchTimestamp :: TimeLocale -> TimeZone -> [T.Text] -> A.Value -> Doc
fetchTimestamp locale timezone [] value =
  maybe empty (timestamp defaultTimeLocale . utcToZonedTime timezone)
  $ parseMaybe parseJSON value
fetchTimestamp locale timezone (x : xs) (A.Object value) =
  maybe empty (fetchTimestamp locale timezone xs)
  $ parseMaybe (.: x) value
fetchTimestamp _ _ _ _ = empty

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

infixr 0 <$$>
(<$$>) :: TemplateSpec -> TemplateSpec -> TemplateSpec
(<$$>) = Union
