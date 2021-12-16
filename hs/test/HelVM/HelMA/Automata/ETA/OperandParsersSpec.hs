module HelVM.HelMA.Automata.ETA.OperandParsersSpec (spec) where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Common.Control.Safe
import           HelVM.Expectations

import qualified Data.Vector                             as Vector

import           Test.Hspec                              (Spec, describe, it)

spec :: Spec
spec =
  describe "parseInteger" $ do
    it "[E]"             $ parseInteger [E]             `shouldSafe` 0
    it "[S , E]"         $ parseInteger [S , E]         `shouldSafe` 6
    it "[T , E]"         $ parseInteger [T , E]         `shouldSafe` 1
    it "[S , S , E]"     $ parseInteger [S , S , E]     `shouldSafe` 48
    it "[S , T , E]"     $ parseInteger [S , T , E]     `shouldSafe` 43
    it "[T , S , E]"     $ parseInteger [T , S , E]     `shouldSafe` 13
    it "[T , T , E]"     $ parseInteger [T , T , E]     `shouldSafe` 8
    it "[S , S , S , E]" $ parseInteger [S , S , S , E] `shouldSafe` 342
    it "[S , S , T , E]" $ parseInteger [S , S , T , E] `shouldSafe` 337
    it "[S , T , S , E]" $ parseInteger [S , T , S , E] `shouldSafe` 307
    it "[S , T , T , E]" $ parseInteger [S , T , T , E] `shouldSafe` 302
    it "[T , S , S , E]" $ parseInteger [T , S , S , E] `shouldSafe` 97
    it "[T , S , T , E]" $ parseInteger [T , S , T , E] `shouldSafe` 92
    it "[T , T , S , E]" $ parseInteger [T , T , S , E] `shouldSafe` 62
    it "[T , T , T , E]" $ parseInteger [T , T , T , E] `shouldSafe` 57

parseInteger :: TokenList -> Safe Integer
parseInteger tl = fst <$> parseNumber (IU (Vector.fromList tl) 0)
