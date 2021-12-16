module HelVM.Common.ListLikeUtilSpec (spec) where

import           HelVM.Common.ListLikeUtil

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "splitBy" $ do
    it "(3) [1,2,3,4,1,2,3,4]" $ splitBy (3 :: Integer) [1,2,3,4,1,2,3,4] `shouldBe` ([1,2] , [4,1,2,3,4])
    it "(9) [1,2,3]"           $ splitBy (9 :: Integer) [1,2,3]           `shouldBe` ([1,2,3] , [])
    it "(0) [1,2,3]"           $ splitBy (9 :: Integer) [1,2,3]           `shouldBe` ([1,2,3] , [])
