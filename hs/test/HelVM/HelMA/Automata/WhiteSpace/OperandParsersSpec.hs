module HelVM.HelMA.Automata.WhiteSpace.OperandParsersSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import HelVM.HelMA.Automata.WhiteSpace.Token

import Test.Hspec

spec :: Spec
spec = do
  describe "(fst . parseNatural)" $ do
    it "[N]"          $ do (fst . parseNatural) [N]          `shouldBe` 0
    it "[S, N]"       $ do (fst . parseNatural) [S, N]       `shouldBe` 0
    it "[T, N]"       $ do (fst . parseNatural) [T, N]       `shouldBe` 1
    it "[S, S, N]"    $ do (fst . parseNatural) [S, S, N]    `shouldBe` 0
    it "[S, T, N]"    $ do (fst . parseNatural) [S, T, N]    `shouldBe` 1
    it "[T, S, N]"    $ do (fst . parseNatural) [T, S, N]    `shouldBe` 2
    it "[T, T, N]"    $ do (fst . parseNatural) [T, T, N]    `shouldBe` 3
    it "[S, S, S, N]" $ do (fst . parseNatural) [S, S, S, N] `shouldBe` 0
    it "[S, S, T, N]" $ do (fst . parseNatural) [S, S, T, N] `shouldBe` 1
    it "[S, T, S, N]" $ do (fst . parseNatural) [S, T, S, N] `shouldBe` 2
    it "[S, T, T, N]" $ do (fst . parseNatural) [S, T, T, N] `shouldBe` 3
    it "[T, S, S, N]" $ do (fst . parseNatural) [T, S, S, N] `shouldBe` 4
    it "[T, S, T, N]" $ do (fst . parseNatural) [T, S, T, N] `shouldBe` 5
    it "[T, T, S, N]" $ do (fst . parseNatural) [T, T, S, N] `shouldBe` 6
    it "[T, T, T, N]" $ do (fst . parseNatural) [T, T, T, N] `shouldBe` 7

  describe "(fst . parseInteger)" $ do
    it "[N]"          $ do (fst . parseInteger) [N]          `shouldBe`   0
    it "[S, N]"       $ do (fst . parseInteger) [S, N]       `shouldBe`   0
    it "[T, N]"       $ do (fst . parseInteger) [T, N]       `shouldBe`   0
    it "[S, S, N]"    $ do (fst . parseInteger) [S, S, N]    `shouldBe`   0
    it "[S, T, N]"    $ do (fst . parseInteger) [S, T, N]    `shouldBe`   1
    it "[T, S, N]"    $ do (fst . parseInteger) [T, S, N]    `shouldBe`   0
    it "[T, T, N]"    $ do (fst . parseInteger) [T, T, N]    `shouldBe` (-1)
    it "[S, S, S, N]" $ do (fst . parseInteger) [S, S, S, N] `shouldBe`   0
    it "[S, S, T, N]" $ do (fst . parseInteger) [S, S, T, N] `shouldBe`   1
    it "[S, T, S, N]" $ do (fst . parseInteger) [S, T, S, N] `shouldBe`   2
    it "[S, T, T, N]" $ do (fst . parseInteger) [S, T, T, N] `shouldBe`   3
    it "[T, S, S, N]" $ do (fst . parseInteger) [T, S, S, N] `shouldBe`   0
    it "[T, S, T, N]" $ do (fst . parseInteger) [T, S, T, N] `shouldBe` (-1)
    it "[T, T, S, N]" $ do (fst . parseInteger) [T, T, S, N] `shouldBe` (-2)
    it "[T, T, T, N]" $ do (fst . parseInteger) [T, T, T, N] `shouldBe` (-3)

  describe "(fst . parseBitString)" $ do
    it "[S, S, S, N]" $ do (fst . parseBitString) [S, S, S, N] `shouldBe` "000"
    it "[S, S, T, N]" $ do (fst . parseBitString) [S, S, T, N] `shouldBe` "001"
    it "[S, T, S, N]" $ do (fst . parseBitString) [S, T, S, N] `shouldBe` "010"
    it "[S, T, T, N]" $ do (fst . parseBitString) [S, T, T, N] `shouldBe` "011"
    it "[T, S, S, N]" $ do (fst . parseBitString) [T, S, S, N] `shouldBe` "100"
    it "[T, S, T, N]" $ do (fst . parseBitString) [T, S, T, N] `shouldBe` "101"
    it "[T, T, S, N]" $ do (fst . parseBitString) [T, T, S, N] `shouldBe` "110"
    it "[T, T, T, N]" $ do (fst . parseBitString) [T, T, T, N] `shouldBe` "111"
