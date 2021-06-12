module HelVM.HelMA.Automata.ETA.OperandParsersSpec (spec) where

import HelVM.HelMA.Automata.ETA.OperandParsers
import HelVM.HelMA.Automata.ETA.Token

import HelVM.Expectations

import HelVM.Common.Safe

import Test.Hspec

spec :: Spec
spec = do
  describe "parseInteger" $ do
    it "[E]"             $ do parseInteger [E]             `shouldSafe` 0
    it "[S , E]"         $ do parseInteger [S , E]         `shouldSafe` 6
    it "[T , E]"         $ do parseInteger [T , E]         `shouldSafe` 1
    it "[S , S , E]"     $ do parseInteger [S , S , E]     `shouldSafe` 48
    it "[S , T , E]"     $ do parseInteger [S , T , E]     `shouldSafe` 43
    it "[T , S , E]"     $ do parseInteger [T , S , E]     `shouldSafe` 13
    it "[T , T , E]"     $ do parseInteger [T , T , E]     `shouldSafe` 8
    it "[S , S , S , E]" $ do parseInteger [S , S , S , E] `shouldSafe` 342
    it "[S , S , T , E]" $ do parseInteger [S , S , T , E] `shouldSafe` 337
    it "[S , T , S , E]" $ do parseInteger [S , T , S , E] `shouldSafe` 307
    it "[S , T , T , E]" $ do parseInteger [S , T , T , E] `shouldSafe` 302
    it "[T , S , S , E]" $ do parseInteger [T , S , S , E] `shouldSafe` 97
    it "[T , S , T , E]" $ do parseInteger [T , S , T , E] `shouldSafe` 92
    it "[T , T , S , E]" $ do parseInteger [T , T , S , E] `shouldSafe` 62
    it "[T , T , T , E]" $ do parseInteger [T , T , T , E] `shouldSafe` 57

parseInteger :: TokenList -> Safe Integer
parseInteger tl = fst <$> parseNumber (IU tl 0)
