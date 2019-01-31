module Tora.Template.Runtime
  ( eval
  ) where

import qualified Data.Aeson                   as A
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Text.PrettyPrint.Leijen.Text hiding ((<$$>), (<$>))
import           Tora.Template.PPrint
import           Tora.Template.Syntax

applyBlockAttrs :: [BlockAttr] -> Doc -> Doc
applyBlockAttrs [] doc              = doc
applyBlockAttrs (Nest n : rest) doc = applyBlockAttrs rest (nest n doc)

splitPath :: T.Text -> [T.Text]
splitPath = T.split (== '.')

eval :: Syntax -> A.Value -> Doc
eval (Block attrs children) value =
  applyBlockAttrs attrs (mconcat $ map (flip eval value) children)
eval (Data attrs rawpath) value =
  let path = splitPath rawpath
  in case attrs of
       [Timestamp] -> fetchTimestamp undefined path value
       [Multiline] -> fetchData path multiline value
       _           -> fetchData path singleline value
eval (Text value) _ = text $ TL.fromStrict value
