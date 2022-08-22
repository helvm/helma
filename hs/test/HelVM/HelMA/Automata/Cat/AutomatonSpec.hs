module HelVM.HelMA.Automata.Cat.AutomatonSpec (spec) where

import           HelVM.HelMA.Automata.Cat.Automaton

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.MockIO

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "output" $
    it "Hello, world!" $ (calculateOutput . execMockIOBatch . eval) hw `shouldBe` toText hw

hw :: Source
hw = "#!/bin/cat\nHello, world!\n"