module HelVM.HelMA.Automata.ETA.Automaton
  ( newMemory
  , runAutomat
  ) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Trampoline        as Trampoline

import           HelVM.HelMA.Automaton.Eff.AutomatonEff

import           HelVM.HelMA.Automaton.Combiner.ALU      as Stack

import           Control.Monad.Extra
import           Control.Type.Operator
import           HelVM.HelMA.Automata.ETA.Symbol

import qualified Data.Vector                             as Vector

import           Prelude                                 hiding ( divMod )

runAutomat ∷ (SAutomatonEff e s m) ⇒ Maybe Natural → Memory s → m $ Memory s
runAutomat = trampolineMWithLimit nextState

nextState ∷ (SAutomatonEff e s m) ⇒ Memory s → SameT m (Memory s)
nextState (Memory iu s) = build =<< nextIM iu where build (t , iu') = doInstruction t (Memory iu' s)

doInstruction ∷ (SAutomatonEff e s m) ⇒ Maybe Token → Memory s → SameT m (Memory s)
-- | IO instructions
doInstruction (Just O) u                        = Trampoline.continueM . updateStack u =<< outputChar (memoryStack u)
doInstruction (Just I) u                        = Trampoline.continueM . updateStack u =<< inputChar (memoryStack u)

-- | Stack instructions
doInstruction (Just N) (Memory iu s)            = build =<< parseNumber iu where build (symbol , iu') = Trampoline.continueM (Memory iu' (push1 symbol s))
doInstruction (Just H) u                        = Trampoline.continueM . updateStack u =<< halibut (memoryStack u)

-- | Arithmetic
doInstruction (Just S) u                        = Trampoline.continueM . updateStack u =<< sub (memoryStack u)
doInstruction (Just E) u                        = Trampoline.continueM . updateStack u =<< divMod (memoryStack u)

-- | Control
doInstruction (Just R) u                        = Trampoline.continueM  u
doInstruction (Just A) (Memory iu@(IM il ic) s) = Trampoline.continueM  ((Memory iu . flipPush1 s . genericNextLabel il) ic)
doInstruction (Just T) u                        = transfer u
doInstruction Nothing u                         = end u

transfer ∷ (SAutomatonEff e s m) ⇒ Memory s → SameT m (Memory s)
transfer = branch <=< pop2ForStack where
  branch (_ , 0 , u) = Trampoline.continueM  u
  branch (0 , _ , u) = end u
  branch (l , _ , u) = Trampoline.continueM . updateAddress u =<< genericFindAddress (memoryProgram u) l

pop2ForStack ∷ (SAutomatonEff e s m) ⇒ Memory s → m (e , e , Memory s)
pop2ForStack u = build <$> pop2 (memoryStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

-- | Terminate instruction
end ∷ (SAutomatonEff e s m) ⇒ Memory s → SameT m (Memory s)
end = Trampoline.breakM

-- | Memory methods

newMemory ∷ TokenList → s → Memory s
newMemory tl = Memory (IM (Vector.fromList tl) 0)

updateStack ∷ Memory s → s → Memory s
updateStack u s =  u {memoryStack = s}

updateAddress ∷ Memory s → InstructionCounter → Memory s
updateAddress u a =  u {memoryIM = updatePC (memoryIM u) a}

memoryProgram ∷ Memory s → TokenVector
memoryProgram = program . memoryIM

-- | Types

data Memory s
  = Memory
      { memoryIM    :: !InstructionMemory
      , memoryStack :: s
      }
  deriving stock (Eq, Read, Show)
