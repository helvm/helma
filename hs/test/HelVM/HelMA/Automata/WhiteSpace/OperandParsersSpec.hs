module HelVM.HelMA.Automata.WhiteSpace.OperandParsersSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.Expectations

import Test.Hspec (Spec , describe , it)

spec :: Spec
spec = do
  describe "(fst <$> parseNatural)" $ do
    it "[N]"             $ do (fst <$> parseNatural [N]            ) `shouldSafe` 0
    it "[S , N]"         $ do (fst <$> parseNatural [S , N]        ) `shouldSafe` 0
    it "[T , N]"         $ do (fst <$> parseNatural [T , N]        ) `shouldSafe` 1
    it "[S , S , N]"     $ do (fst <$> parseNatural [S , S , N]    ) `shouldSafe` 0
    it "[S , T , N]"     $ do (fst <$> parseNatural [S , T , N]    ) `shouldSafe` 1
    it "[T , S , N]"     $ do (fst <$> parseNatural [T , S , N]    ) `shouldSafe` 2
    it "[T , T , N]"     $ do (fst <$> parseNatural [T , T , N]    ) `shouldSafe` 3
    it "[S , S , S , N]" $ do (fst <$> parseNatural [S , S , S , N]) `shouldSafe` 0
    it "[S , S , T , N]" $ do (fst <$> parseNatural [S , S , T , N]) `shouldSafe` 1
    it "[S , T , S , N]" $ do (fst <$> parseNatural [S , T , S , N]) `shouldSafe` 2
    it "[S , T , T , N]" $ do (fst <$> parseNatural [S , T , T , N]) `shouldSafe` 3
    it "[T , S , S , N]" $ do (fst <$> parseNatural [T , S , S , N]) `shouldSafe` 4
    it "[T , S , T , N]" $ do (fst <$> parseNatural [T , S , T , N]) `shouldSafe` 5
    it "[T , T , S , N]" $ do (fst <$> parseNatural [T , T , S , N]) `shouldSafe` 6
    it "[T , T , T , N]" $ do (fst <$> parseNatural [T , T , T , N]) `shouldSafe` 7

  describe "(fst <$> parseInteger)" $ do
    it "[N]"             $ do (fst <$> parseInteger [N]            ) `shouldSafe`   0
    it "[S , N]"         $ do (fst <$> parseInteger [S , N]        ) `shouldSafe`   0
    it "[T , N]"         $ do (fst <$> parseInteger [T , N]        ) `shouldSafe`   0
    it "[S , S , N]"     $ do (fst <$> parseInteger [S , S , N]    ) `shouldSafe`   0
    it "[S , T , N]"     $ do (fst <$> parseInteger [S , T , N]    ) `shouldSafe`   1
    it "[T , S , N]"     $ do (fst <$> parseInteger [T , S , N]    ) `shouldSafe`   0
    it "[T , T , N]"     $ do (fst <$> parseInteger [T , T , N]    ) `shouldSafe` (-1)
    it "[S , S , S , N]" $ do (fst <$> parseInteger [S , S , S , N]) `shouldSafe`   0
    it "[S , S , T , N]" $ do (fst <$> parseInteger [S , S , T , N]) `shouldSafe`   1
    it "[S , T , S , N]" $ do (fst <$> parseInteger [S , T , S , N]) `shouldSafe`   2
    it "[S , T , T , N]" $ do (fst <$> parseInteger [S , T , T , N]) `shouldSafe`   3
    it "[T , S , S , N]" $ do (fst <$> parseInteger [T , S , S , N]) `shouldSafe`   0
    it "[T , S , T , N]" $ do (fst <$> parseInteger [T , S , T , N]) `shouldSafe` (-1)
    it "[T , T , S , N]" $ do (fst <$> parseInteger [T , T , S , N]) `shouldSafe` (-2)
    it "[T , T , T , N]" $ do (fst <$> parseInteger [T , T , T , N]) `shouldSafe` (-3)

  describe "(fst <$> parseDigitString)" $ do
    it "[S , S , S , N]" $ do (fst <$> parseDigitString [S , S , S , N]) `shouldSafe` "000"
    it "[S , S , T , N]" $ do (fst <$> parseDigitString [S , S , T , N]) `shouldSafe` "001"
    it "[S , T , S , N]" $ do (fst <$> parseDigitString [S , T , S , N]) `shouldSafe` "010"
    it "[S , T , T , N]" $ do (fst <$> parseDigitString [S , T , T , N]) `shouldSafe` "011"
    it "[T , S , S , N]" $ do (fst <$> parseDigitString [T , S , S , N]) `shouldSafe` "100"
    it "[T , S , T , N]" $ do (fst <$> parseDigitString [T , S , T , N]) `shouldSafe` "101"
    it "[T , T , S , N]" $ do (fst <$> parseDigitString [T , T , S , N]) `shouldSafe` "110"
    it "[T , T , T , N]" $ do (fst <$> parseDigitString [T , T , T , N]) `shouldSafe` "111"
