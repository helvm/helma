module HelVM.Common.ListLikeUtilSpec (spec) where

import           HelVM.Common.ListLikeUtil

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "splitBy" $
    forM_ [ ("(3) [1,2,3,4,1,2,3,4]" , (3 , [1,2,3,4,1,2,3,4] , ([1,2]   , [4,1,2,3,4])))
          , ("(9) [1,2,3]"           , (9 , [1,2,3]           , ([1,2,3] , []         )))
          , ("(0) [1,2,3]"           , (9 , [1,2,3]           , ([1,2,3] , []         )))
          ] $ \(name , (by , input , output)) ->
      it name $ splitBy (by :: Integer) input `shouldBe` output
