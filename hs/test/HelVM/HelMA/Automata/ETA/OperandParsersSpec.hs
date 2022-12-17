module HelVM.HelMA.Automata.ETA.OperandParsersSpec (spec) where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Optimizer
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction

import           HelVM.Expectations
import           HelVM.HelIO.Control.Safe

import qualified Data.Vector                                     as Vector

import           Test.Hspec                                      (Spec, describe, it)

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
    [ (E , [IAL (Binaries [Mod,Div])])
    , (T , [Transfer]                )
    , (A , [IAL (Cons 2)]            )
    , (O , [IAL (SIO OutputChar)]    )
    , (I , [IAL (SIO InputChar)]     )
    , (S , [IAL (Binary Sub)]        )
    , (H , [IAL Halibut]             )
    , (R , [ICF (DMark 2)]           )
    ] $ \(input , output) ->
    describe (show input) $
      it "optimalize" $ optimize [input] `shouldSafe` decorateIL output

parseInteger :: TokenList -> Safe Integer
parseInteger tl = fst <$> parseNumber (IU (Vector.fromList tl) 0)

decorateInteger :: Integer -> [Instruction]
decorateInteger i = decorateIL [IAL (Cons i)]

decorateIL :: [Instruction] -> [Instruction]
decorateIL il = [ICF (DMark 1)] <> il <> [End]
