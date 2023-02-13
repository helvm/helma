module HelVM.HelMA.Automaton.OptimizerSpec (spec) where

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import           Test.Hspec                                  (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "optimize" $ forM_
    [ ([consI 1 , consI 2 , consI 3] , [consI 1 , consI 2 , consI 3])
    , ([consI 1 , consI 2 , addI] , [consI 3])
    , ([halibutI] , [halibutI])
    , ([consI 1 , consI 0 , halibutI] , [consI 1 , consI 1])
    , ([consI 1 , halibutI] , [moveII 1])
    , ([consI 2 , halibutI] , [moveII 2])
    , ([consI 0 , halibutI] , [copyII 0])
    , ([consI $ negate 1 , halibutI] , [copyII 1])
    , ([consI 1 , moveII 1 , bNeTI] , [jumpTI])
    , ([consI 0 , moveII 1 , bNeTI] , [discardI])
    ] $ \(input , output) ->
    it (show input) $ optimize AllOptimizations input `shouldBe` output
