{-# LANGUAGE OverloadedStrings #-}

module Tora.Config
 ( Config ()
 , readConfig
 , template
 , index
 ) where

import           Data.Aeson           ((.:), (.:?), (.=))
import qualified Data.Aeson           as A
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Yaml            as Y
import           Tora.Template.Syntax
import Tora.Template.Compiler
import Tora.Template.Runtime

data Config
  = Config { templates :: M.HashMap T.Text Syntax
           , indices   :: M.HashMap T.Text T.Text
           }
  deriving (Show)

readConfig :: FilePath -> IO (Maybe Config)
readConfig path = either (const Nothing) Just <$> Y.decodeFileEither path

template :: T.Text -> Config -> Maybe Syntax
template key = M.lookup key . templates

index :: T.Text -> Config -> Maybe T.Text
index key = M.lookup key . indices

parseTemplate :: M.HashMap T.Text T.Text -> M.HashMap T.Text Syntax
parseTemplate = M.mapMaybe compile

instance A.FromJSON Config where
  parseJSON =
    A.withObject "config" $ \obj ->
      Config
      <$> (parseTemplate <$> (obj .: "templates"))
      <*> (obj .: "indices")
