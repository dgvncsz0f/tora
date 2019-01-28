module Tora.Output where

import           Control.Monad.Reader
import qualified Data.Aeson                   as A
import           Text.PrettyPrint.Leijen.Text
import           Tora.Input
import           Tora.PPrint
import           Tora.Types

display :: Reader A.Value Doc -> Maybe SearchResult -> IO (ReduceResult ())
display _ Nothing = pure $ Halt ()
display template (Just result) = do
  mapM_ (printDoc . runReader template) (_prHits result)
  pure $ Cont () (_prCursor result)

printDoc :: Doc -> IO ()
printDoc doc = do
  putDoc doc
  putStrLn ""
  putStrLn ""
