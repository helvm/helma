module HelVM.HelMA.Automaton.OptimizerSpec (spec) where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import           Test.Hspec                        (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "constantFolding" $ forM_
    [ ([consI 1 , consI 2 , consI 3] , [consI 1 , consI 2 , consI 3])
    , ([consI 1 , consI 2 , addI] , [consI 3])
    , ([halibutI] , [halibutI])
    , ([consI 0 , halibutI] , [consI 0 , halibutI])
    , ([consI 1 , halibutI] , [consI 1 , halibutI])
    , ([consI 2 , halibutI] , [consI 2 , halibutI])
    , ([consI 1 , consI 0 , halibutI] , [consI 1 , consI 1])
    ] $ \(input , output) ->
    it (show input) $ constantFolding input `shouldBe` output
