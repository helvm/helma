module HelVM.HelMA.Automaton.Memories.Stack.LLStackSpec (spec)  where

import           HelVM.HelMA.Automaton.Memories.Stack.MTStack
import           HelVM.HelMA.Automaton.Operator.StackOperator

import           HelVM.Expectations

import           Test.Hspec                                   (Spec, describe, it)

spec :: Spec
spec =
  describe "LLStack" $
    forM_ [ (Move 0 , [11 , 22 , 33 :: Integer] , [11 , 22 , 33 :: Integer] )
          , (Move 1 , [11 , 22 , 33] , [22 , 11 , 33])
          , (Move 2 , [11 , 22 , 33] , [33 , 11 , 22])
          , (Move 3 , [11 , 22 , 33] , [11 , 22 , 33])
          , (Swap   , [11 , 22 , 33] , [22 , 11 , 33])
          , (Rot    , [11 , 22 , 33] , [33 , 11 , 22])

          , (Copy 0 , [11 , 22 , 33] , [11 , 11 , 22 , 33])
          , (Copy 1 , [11 , 22 , 33] , [22 , 11 , 22 , 33])
          , (Copy 2 , [11 , 22 , 33] , [33 , 11 , 22 , 33])
          , (DCopy  , [0 , 11 , 22 , 33] , [11 , 11 , 22 , 33])

          ] $ \(operator , input , output) ->
      it (show operator) $ stackOp operator input `shouldSafe` output

