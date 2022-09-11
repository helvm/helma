module HelVM.HelMA.Automata.LazyK.NumberSpec (spec) where

import           HelVM.HelMA.Automata.LazyK.InputEncoder

import           HelVM.HelMA.Automata.LazyK.Evaluator
import           HelVM.HelMA.Automata.LazyK.Reducer

import           HelVM.HelIO.Control.Safe

import           HelVM.Expectations

import           Test.Hspec                              (Spec, describe, it)

spec :: Spec
spec =
  describe "special church" $ forM_
    [0, 1, 4, 8, 9, 16, 27, 36, 64, 81, 100, 121, 125, 256] $ \number ->
    it ("special church " <> show number) $
       f number `shouldSafe` number

f :: MonadSafe m => Natural -> m Natural
f = realize . reduce . church
