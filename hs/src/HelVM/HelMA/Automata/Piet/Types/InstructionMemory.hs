module HelVM.HelMA.Automata.Piet.Types.InstructionMemory
  ( InstructionMemory (..)
  , codelChooserIM
  , directionPointerIM
  , initialInstructionMemory
  , logWithPosition
  , rotateDirectionPointerIM
  , toggleCodelChooserIM
  ) where


import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

logWithPosition ∷ AppSafeEff m ⇒ Text → InstructionMemory → m ()
logWithPosition msg im = logDebugN $ show (position $ instructionCounter im) <> " " <> msg

codelChooserIM ∷ InstructionMemory → DirectionPointer
codelChooserIM = directionPointerIC .  instructionCounter

directionPointerIM ∷ InstructionMemory → DirectionPointer
directionPointerIM = directionPointerIC .  instructionCounter

toggleCodelChooserIM ∷ Int → InstructionMemory → InstructionMemory
toggleCodelChooserIM n im = im { instructionCounter = toggleCodelChooserIC n (instructionCounter im)}

rotateDirectionPointerIM ∷ Int → InstructionMemory → InstructionMemory
rotateDirectionPointerIM n im = im { instructionCounter = rotateDirectionPointerIC n (instructionCounter im)}

initialInstructionMemory ∷ Program → InstructionMemory
initialInstructionMemory prog = InstructionMemory
  { instructionCounter = initialInstructionCounter
  , program            = prog
  }

data InstructionMemory
  = InstructionMemory
      { instructionCounter :: !InstructionCounter
      , program            :: !Program
      }
