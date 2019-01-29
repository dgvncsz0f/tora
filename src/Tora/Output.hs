module Tora.Output
  ( printDoc
  , displayTailResult
  , displayInfoResult
  ) where

import           Control.Monad.Reader
import qualified Data.Aeson                   as A
import           Text.PrettyPrint.Leijen.Text
import           Tora.Input
import           Tora.PPrint
import           Tora.Types

displayTailResult :: Reader A.Value Doc -> TailResult -> IO (ReduceResult ())
displayTailResult template result = do
  mapM_ (printDoc . runReader template) (trstHits result)
  pure $ Cont () (trstCursor result)

displayInfoResult :: Bool -> InfoResult -> IO ()
displayInfoResult True  = printDoc . renderInfoResultNoFields
displayInfoResult False = printDoc . renderInfoResult

printDoc :: Doc -> IO ()
printDoc doc = do
  putDoc doc
  putStrLn ""
  putStrLn ""
