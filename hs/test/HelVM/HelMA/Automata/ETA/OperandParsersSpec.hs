module HelVM.HelMA.Automata.ETA.OperandParsersSpec (spec) where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Common.Control.Safe
import           HelVM.Expectations

import qualified Data.Vector                             as Vector

import           Test.Hspec                              (Spec, describe, it)

spec :: Spec
spec =
  describe "parseInteger" $
    forM_ [ ([E]             , 0)
          , ([S , E]         , 6)
          , ([T , E]         , 1)
          , ([S , S , E]     , 48)
          , ([S , T , E]     , 43)
          , ([T , S , E]     , 13)
          , ([T , T , E]     , 8)
          , ([S , S , S , E] , 342)
          , ([S , S , T , E] , 337)
          , ([S , T , S , E] , 307)
          , ([S , T , T , E] , 302)
          , ([T , S , S , E] , 97)
          , ([T , S , T , E] , 92)
          , ([T , T , S , E] , 62)
          , ([T , T , T , E] , 57)
          ] $ \(input , output) ->
      it (show input) $ parseInteger input `shouldSafe` output

parseInteger :: TokenList -> Safe Integer
parseInteger tl = fst <$> parseNumber (IU (Vector.fromList tl) 0)
