module HelVM.HelMA.Automata.Piet.Combiner.CPU
  ( pietPointer
  , pietSwitch
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory

import           HelVM.HelMA.Automaton.Combiner.ALU

import qualified Data.Sequence                                     as Seq

pietPointer ∷ (ALU m (Seq.Seq Int) Int) ⇒ Memory → m Memory
pietPointer = modifyIM "pointer" directionPointerIM rotateDirectionPointerIM

pietSwitch ∷ (ALU m (Seq.Seq Int) Int) ⇒ Memory → m Memory
pietSwitch = modifyIM "switch" codelChooserIM toggleCodelChooserIM

modifyIM ∷ (ALU m (Seq.Seq Int) Int, Show a) ⇒ Text → (InstructionMemory → a) → (Int → InstructionMemory → InstructionMemory) → Memory → m Memory
modifyIM name getValue f (Memory im s) = logWithPosition (name <> " " <> show (getValue im)) im *> (updateMemory <$> pop1 s) where
  updateMemory (n, s') = Memory (f n im) s'
