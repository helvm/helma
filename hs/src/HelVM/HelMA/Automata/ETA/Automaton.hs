module HelVM.HelMA.Automata.ETA.Automaton (
  run,
  newAutomaton,
) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Loop

import           HelVM.HelMA.Automaton.IO.AutomatonIO

import           HelVM.HelMA.Automaton.Units.ALU         as Stack

import           Control.Monad.Extra
import           Control.Type.Operator
import           HelVM.HelMA.Automata.ETA.Symbol

import qualified Data.Vector                             as Vector

import           Prelude                                 hiding (divMod)

run :: (SAutomatonIO e s m) => Maybe Natural -> Automaton s -> m $ Automaton s
run = loopMWithLimit nextState

nextState :: (SAutomatonIO e s m) => Automaton s -> m $ AutomatonSame s
nextState (Automaton iu s) = build =<< nextIU iu where build (t , iu') = doInstruction t (Automaton iu' s)

doInstruction :: (SAutomatonIO e s m) => Maybe Token -> Automaton s -> m $ AutomatonSame s
-- | IO instructions
doInstruction (Just O) u                           = Left . updateStack u <$> doOutputChar2 (unitStack u)
doInstruction (Just I) u                           = Left . updateStack u <$> doInputChar2 (unitStack u)

-- | Stack instructions
doInstruction (Just N) (Automaton iu s)            = build <$> parseNumber iu where build (symbol , iu') = Left (Automaton iu' (push1 symbol s))
doInstruction (Just H) u                           = Left . updateStack u <$> halibut (unitStack u)

-- | Arithmetic
doInstruction (Just S) u                           = Left . updateStack u <$> sub (unitStack u)
doInstruction (Just E) u                           = Left . updateStack u <$> divMod (unitStack u)

-- | Control
doInstruction (Just R) u                           = pure $ Left u
doInstruction (Just A) (Automaton iu@(IU il ic) s) = pure $ Left ((Automaton iu . flipPush1 s . genericNextLabel il) ic)
doInstruction (Just T) u                           = transfer u
doInstruction Nothing u                            = end u

transfer :: (SAutomatonIO e s m) => Automaton s -> m $ AutomatonSame s
transfer = branch <=< pop2ForStack where
  branch (_ , 0 , u) = pure $ Left u
  branch (0 , _ , u) = end u
  branch (l , _ , u) = Left . updateAddress u <$> genericFindAddress (unitProgram u) l

pop2ForStack :: (SAutomatonIO e s m) => Automaton s -> m (e , e , Automaton s)
pop2ForStack u = build <$> pop2 (unitStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

-- | Terminate instruction
end :: (SAutomatonIO e s m) => Automaton s -> m $ AutomatonSame s
end = pure . Right

-- | Automaton methods

newAutomaton :: TokenList -> s -> Automaton s
newAutomaton tl = Automaton (IU (Vector.fromList tl) 0)

updateStack :: Automaton s -> s -> Automaton s
updateStack u s =  u {unitStack = s}

updateAddress :: Automaton s -> InstructionCounter -> Automaton s
updateAddress u a =  u {unitIU = updatePC (unitIU u) a}

unitProgram :: Automaton s -> TokenVector
unitProgram = program . unitIU

-- | Types

type AutomatonSame s = Same (Automaton s)

data Automaton s = Automaton
  { unitIU    :: !InstructionUnit
  , unitStack :: s
  }
  deriving stock (Eq , Read , Show)
