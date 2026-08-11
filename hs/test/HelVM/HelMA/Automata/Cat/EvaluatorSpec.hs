module HelVM.HelMA.Automata.Cat.EvaluatorSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Cat.Evaluator

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.Mock

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec ∷ Spec
spec =
  describe "output" $
    it "Hello, world!" $ (calculateOutput . execMockEffBatch . eval) hw `shouldBe` toText hw

hw ∷ Source
hw = "#!/bin/cat\nHello, world!\n"
