module HelVM.HelMA.Automata.WhiteSpace.OperandParsersSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import HelVM.HelMA.Automata.WhiteSpace.Token
import HelVM.Common.Safe

import Test.Hspec

spec :: Spec
spec = do
  describe "(fst <$> parseNatural)" $ do
    it "[N]"             $ do (fst <$> parseNatural [N]            ) `shouldBe` safe 0
    it "[S , N]"         $ do (fst <$> parseNatural [S , N]        ) `shouldBe` safe 0
    it "[T , N]"         $ do (fst <$> parseNatural [T , N]        ) `shouldBe` safe 1
    it "[S , S , N]"     $ do (fst <$> parseNatural [S , S , N]    ) `shouldBe` safe 0
    it "[S , T , N]"     $ do (fst <$> parseNatural [S , T , N]    ) `shouldBe` safe 1
    it "[T , S , N]"     $ do (fst <$> parseNatural [T , S , N]    ) `shouldBe` safe 2
    it "[T , T , N]"     $ do (fst <$> parseNatural [T , T , N]    ) `shouldBe` safe 3
    it "[S , S , S , N]" $ do (fst <$> parseNatural [S , S , S , N]) `shouldBe` safe 0
    it "[S , S , T , N]" $ do (fst <$> parseNatural [S , S , T , N]) `shouldBe` safe 1
    it "[S , T , S , N]" $ do (fst <$> parseNatural [S , T , S , N]) `shouldBe` safe 2
    it "[S , T , T , N]" $ do (fst <$> parseNatural [S , T , T , N]) `shouldBe` safe 3
    it "[T , S , S , N]" $ do (fst <$> parseNatural [T , S , S , N]) `shouldBe` safe 4
    it "[T , S , T , N]" $ do (fst <$> parseNatural [T , S , T , N]) `shouldBe` safe 5
    it "[T , T , S , N]" $ do (fst <$> parseNatural [T , T , S , N]) `shouldBe` safe 6
    it "[T , T , T , N]" $ do (fst <$> parseNatural [T , T , T , N]) `shouldBe` safe 7

  describe "(fst <$> parseInteger)" $ do
    it "[N]"             $ do (fst <$> parseInteger [N]            ) `shouldBe` safe   0
    it "[S , N]"         $ do (fst <$> parseInteger [S , N]        ) `shouldBe` safe   0
    it "[T , N]"         $ do (fst <$> parseInteger [T , N]        ) `shouldBe` safe   0
    it "[S , S , N]"     $ do (fst <$> parseInteger [S , S , N]    ) `shouldBe` safe   0
    it "[S , T , N]"     $ do (fst <$> parseInteger [S , T , N]    ) `shouldBe` safe   1
    it "[T , S , N]"     $ do (fst <$> parseInteger [T , S , N]    ) `shouldBe` safe   0
    it "[T , T , N]"     $ do (fst <$> parseInteger [T , T , N]    ) `shouldBe` safe (-1)
    it "[S , S , S , N]" $ do (fst <$> parseInteger [S , S , S , N]) `shouldBe` safe   0
    it "[S , S , T , N]" $ do (fst <$> parseInteger [S , S , T , N]) `shouldBe` safe   1
    it "[S , T , S , N]" $ do (fst <$> parseInteger [S , T , S , N]) `shouldBe` safe   2
    it "[S , T , T , N]" $ do (fst <$> parseInteger [S , T , T , N]) `shouldBe` safe   3
    it "[T , S , S , N]" $ do (fst <$> parseInteger [T , S , S , N]) `shouldBe` safe   0
    it "[T , S , T , N]" $ do (fst <$> parseInteger [T , S , T , N]) `shouldBe` safe (-1)
    it "[T , T , S , N]" $ do (fst <$> parseInteger [T , T , S , N]) `shouldBe` safe (-2)
    it "[T , T , T , N]" $ do (fst <$> parseInteger [T , T , T , N]) `shouldBe` safe (-3)

  describe "(fst <$> parseDigitString)" $ do
    it "[S , S , S , N]" $ do (fst <$> parseDigitString [S , S , S , N]) `shouldBe` safe "000"
    it "[S , S , T , N]" $ do (fst <$> parseDigitString [S , S , T , N]) `shouldBe` safe "001"
    it "[S , T , S , N]" $ do (fst <$> parseDigitString [S , T , S , N]) `shouldBe` safe "010"
    it "[S , T , T , N]" $ do (fst <$> parseDigitString [S , T , T , N]) `shouldBe` safe "011"
    it "[T , S , S , N]" $ do (fst <$> parseDigitString [T , S , S , N]) `shouldBe` safe "100"
    it "[T , S , T , N]" $ do (fst <$> parseDigitString [T , S , T , N]) `shouldBe` safe "101"
    it "[T , T , S , N]" $ do (fst <$> parseDigitString [T , T , S , N]) `shouldBe` safe "110"
    it "[T , T , T , N]" $ do (fst <$> parseDigitString [T , T , T , N]) `shouldBe` safe "111"
