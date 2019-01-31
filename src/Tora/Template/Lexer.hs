{-# LANGUAGE OverloadedStrings #-}

module Tora.Template.Lexer
  ( ToraToken(..)
  , Parser
  , lexer
  ) where

import           Data.Char
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data ToraToken
  = LParen
  | RParen
  | Symbol T.Text
  | String T.Text
  | Eof
  deriving (Eq, Show)

nextToken :: Parser ToraToken
nextToken = do
  (single '(' *> pure LParen)
  <|> (single ')' *> pure RParen)
  <|> (single '"' *> (String . T.pack <$> (manyTill L.charLiteral (single '"'))))
  <|> (Symbol . T.pack <$> some (satisfy (\c -> isPrint c && not (isSpace c || c `elem` ['(', ')', '"']))))
  <|> (eof *> pure Eof)

lexer :: Parser ToraToken
lexer = L.lexeme space nextToken
