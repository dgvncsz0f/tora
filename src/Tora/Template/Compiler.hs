{-# LANGUAGE OverloadedStrings #-}

module Tora.Template.Compiler
  ( compile
  ) where

import           Control.Monad.State
import           Data.Char
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Tora.Template.Lexer
import           Tora.Template.Syntax

accept :: (ToraToken -> Maybe a) -> Parser a
accept f = do
  tk <- lexer
  case f tk of
    Nothing -> empty
    Just x  -> pure x

matchSymbol :: T.Text -> ToraToken -> Maybe T.Text
matchSymbol name (Symbol name')
  | name == name' || (name <> ".") `T.isPrefixOf` name' = Just name'
matchRsymbol _ _ = Nothing

matchAnyString :: ToraToken -> Maybe T.Text
matchAnyString (String t) = Just t
matchAnyString _          = Nothing

matchToken :: ToraToken -> ToraToken -> Maybe ()
matchToken tk tk'
  | tk == tk' = pure ()
  | otherwise = Nothing

function :: T.Text -> (T.Text -> a) -> (a -> Parser b) -> Parser b
function symbol attrf p = do
  accept (matchToken LParen)
  sym <- accept (matchSymbol symbol)
  p (attrf sym) <* accept (matchToken RParen)

function_ :: T.Text -> Parser a -> Parser a
function_ symbol p = function symbol (const ()) (const p)

nestAttr :: T.Text -> Maybe BlockAttr
nestAttr name
  | "nest-" `T.isPrefixOf` name =
    case decimal (T.drop 5 name) of
      Right (n, "") -> Just $ Nest n
      Left _        -> Nothing
  | otherwise = Nothing

blockAttr :: T.Text -> [BlockAttr]
blockAttr = build . tail . T.split (== '.')
  where
    build [] = []
    build (item : rest) =
      maybe (build rest) (: build rest) (nestAttr item)

dataAttr :: T.Text -> [DataAttr]
dataAttr = build . tail . T.split (== '.')
  where
    build []                   = []
    build ("timestamp" : rest) = Timestamp : build rest
    build ("multiline" : rest) = Multiline : build rest
    build (_ : rest)           = build rest

data_ :: Parser Syntax
data_ = function "data" dataAttr $ \attrs ->
  Data attrs <$> accept matchAnyString

text_ :: Parser Syntax
text_ = function_ "text" $
  Text <$> accept matchAnyString

block :: Parser Syntax
block = function "block" blockAttr $ \attrs ->
  Block attrs <$> many (try (data_ <|> text_))

template :: Parser Syntax
template = function_ "template" $
  Block [] <$> many (try block)

compile :: T.Text -> Maybe Syntax
compile = parseMaybe template
