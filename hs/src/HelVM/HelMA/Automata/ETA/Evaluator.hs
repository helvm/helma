module HelVM.HelMA.Automata.ETA.Evaluator (
  newUnit,
  next,
) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Units.ALU         as Stack

import           Control.Monad.Extra
import           Control.Type.Operator
import           HelVM.HelMA.Automata.ETA.Symbol

import qualified Data.Vector                             as Vector

import           Prelude                                 hiding (divMod)

next :: (SEvaluator e s m) => Unit s -> m $ Unit s
next = loopM act

act :: (SEvaluator e s m) => Unit s -> m $ UnitBoth s
act (Unit iu s) = build =<< nextIU iu where build (t , iu') = doInstruction t (Unit iu' s)

doInstruction :: (SEvaluator e s m) => Maybe Token -> Unit s -> m $ UnitBoth s
-- | IO instructions
doInstruction (Just O) u                      = Left . updateStack u <$> doOutputChar2 (unitStack u)
doInstruction (Just I) u                      = Left . updateStack u <$> doInputChar2 (unitStack u)

-- | Stack instructions
doInstruction (Just N) (Unit iu s)            = build <$> parseNumber iu where build (symbol , iu') = Left (Unit iu' (push1 symbol s))
doInstruction (Just H) u                      = Left . updateStack u <$> halibut (unitStack u)

-- | Arithmetic
doInstruction (Just S) u                      = Left . updateStack u <$> sub (unitStack u)
doInstruction (Just E) u                      = Left . updateStack u <$> divMod (unitStack u)

-- | Control
doInstruction (Just R) u                      = pure $ Left u
doInstruction (Just A) (Unit iu@(IU il ic) s) = pure $ Left ((Unit iu . flipPush1 s . genericNextLabel il) ic)
doInstruction (Just T) u                      = transfer u
doInstruction Nothing u                       = end u

transfer :: (SEvaluator e s m) => Unit s -> m $ UnitBoth s
transfer = branch <=< pop2ForStack where
  branch (_ , 0 , u) = pure $ Left u
  branch (0 , _ , u) = end u
  branch (l , _ , u) = Left . updateAddress u <$> genericFindAddress (unitProgram u) l

pop2ForStack :: (SEvaluator e s m) => Unit s -> m (e , e , Unit s)
pop2ForStack u = build <$> pop2 (unitStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

-- | Terminate instruction
end :: (SEvaluator e s m) => Unit s -> m $ UnitBoth s
end = pure . Right

-- | Unit methods

newUnit :: TokenList -> s -> Unit s
newUnit tl = Unit (IU (Vector.fromList tl) 0)

updateStack :: Unit s -> s -> Unit s
updateStack u s =  u {unitStack = s}

updateAddress :: Unit s -> InstructionCounter -> Unit s
updateAddress u a =  u {unitIU = updatePC (unitIU u) a}

unitProgram :: Unit s -> TokenVector
unitProgram = program . unitIU

-- | Types

type UnitBoth s = Both (Unit s)

data Unit s = Unit
  { unitIU    :: !InstructionUnit
  , unitStack :: s
  }
  deriving stock (Eq , Read , Show)

type Both a = Either a a
