module Tora.Template.Syntax
  ( BlockAttr(..)
  , DataAttr(..)
  , Syntax (..)
  ) where

import qualified Data.Text as T

data BlockAttr
  = Nest Int
  deriving (Show)

data DataAttr
  = Timestamp
  | Multiline
  deriving (Show)

data Syntax
  = Block [BlockAttr] [Syntax]
  | Data [DataAttr] T.Text
  | Text T.Text
  deriving (Show)
