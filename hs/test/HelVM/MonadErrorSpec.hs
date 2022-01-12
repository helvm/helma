module HelVM.MonadErrorSpec (spec) where

import           Control.Monad.Except

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "MonadError" $
    forM_ [ ("Right", textRight, Right 1   )
          , ("Left" , textLeft , Left "one")
          ] $ \(name , input , output) ->
      it name $ input `shouldBe` output

type MonadTextError m = MonadError Text m

textRight :: MonadTextError m => m Integer
textRight = pure 1

textLeft :: MonadTextError m => m Integer
textLeft = throwError "one"
