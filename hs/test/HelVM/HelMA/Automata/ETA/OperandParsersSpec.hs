module HelVM.HelMA.Automata.ETA.OperandParsersSpec (spec) where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Optimizer
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.Expectations
import           HelVM.HelIO.Control.Safe

import qualified Data.Vector                             as Vector

import           Test.Hspec                              (Spec, describe, it)

spec :: Spec
spec = describe "parse" $ do
  describe "parseInteger" $ forM_
    [ ([E]             , 0)
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
    describe (show input) $ do
      it "parseInteger" $ parseInteger input `shouldSafe` output
      it "optimalize" $ optimize ([N] <> input) `shouldSafe` decorateInteger output
  describe "single" $ forM_
    [ (E , [divModI, markSI "1"])
    , (T , [bNeTI              ])
    , (A , [consI 2            ])
    , (O , [sOutputI           ])
    , (I , [sInputI            ])
    , (S , [subI               ])
    , (H , [halibutI           ])
    , (R , [markNI 2           ])
    ] $ \(input , output) ->
    describe (show input) $
      it "optimalize" $ optimize [input] `shouldSafe` decorateIL output

parseInteger :: TokenList -> Safe Integer
parseInteger tl = fst <$> parseNumber (IM (Vector.fromList tl) 0)

decorateInteger :: Integer -> [Instruction]
decorateInteger i = decorateIL [consI i]

decorateIL :: [Instruction] -> [Instruction]
decorateIL il = [markNI 1] <> il <> [markNI 0 , End]
