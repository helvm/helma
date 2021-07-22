module HelVM.MonadErrorSpec (spec) where

import           Control.Monad.Except

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "MonadError" $ do
    it "Right" $ do textRight `shouldBe` Right 1
    it "Left"  $ do textLeft  `shouldBe` Left "one"

type MonadTextError m = MonadError Text m

textRight :: MonadTextError m => m Integer
textRight = pure 1

textLeft :: MonadTextError m => m Integer
textLeft = throwError "one"
