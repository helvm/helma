module HelVM.Common.Control.LoggerSpec (spec) where

import           HelVM.Common.Control.Logger

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

import qualified Data.DList                     as D

spec :: Spec
spec =
  describe "logger" $
    forM_ [ ("logger"     ,                                logger () , D.empty                     )
          , ("liftLogger" ,                   liftLogger $ logger () , D.empty                     )
          , ("Hello"      , logMessage "Hello"                       , D.fromList["Hello"]         )
          , ("World"      , logMessage "Hello" <* logMessage "World" , D.fromList["Hello", "World"])
          ] $ \ (name , action, logs) ->
      it name $ logsFromLogger action `shouldBe` logs
