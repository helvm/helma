module HelVM.HelMA.Automata.Rev.AutomatonSpec (spec) where

import           HelVM.HelMA.Automata.Rev.Automaton

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.MockIO

import           Test.Hspec                         (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "output" $
    it "Hello, world!" $ (calculateOutput . execMockIOBatch . run) hw `shouldBe` hwo

hw :: Source
hw = "#!/usr/bin/rev\n!dlrow ,olleH\n"

hwo :: Output
hwo = "ver/nib/rsu/!#\nHello, world!\n"
