module HelVM.HelMA.Automata.Piet.Types.InstructionMemory
  ( InstructionMemory (..)
  , codelChooserIM
  , directionPointerIM
  , initialInstructionMemory
  , instructionCounter
  , logWithPosition
  , program
  , rotateDirectionPointerIM
  , toggleCodelChooserIM
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import           Lens.Micro                                         ( (%~), (^.) )
import           Lens.Micro.TH                                      ( makeLenses )

data InstructionMemory
  = InstructionMemory
      { _instructionCounter :: !InstructionCounter
      , _program            :: !Program
      }

makeLenses ''InstructionMemory

logWithPosition ∷ AppSafeEff m ⇒ Text → InstructionMemory → m ()
logWithPosition msg im = logDebugN $ show (im ^. instructionCounter . position) <> " " <> msg

codelChooserIM ∷ InstructionMemory → CodelChooser
codelChooserIM im = codelChooserIC (im ^. instructionCounter)

directionPointerIM ∷ InstructionMemory → DirectionPointer
directionPointerIM im = directionPointerIC (im ^. instructionCounter)

toggleCodelChooserIM ∷ Int → InstructionMemory → InstructionMemory
toggleCodelChooserIM n = instructionCounter %~ toggleCodelChooserIC n

rotateDirectionPointerIM ∷ Int → InstructionMemory → InstructionMemory
rotateDirectionPointerIM n = instructionCounter %~ rotateDirectionPointerIC n

initialInstructionMemory ∷ Program → InstructionMemory
initialInstructionMemory prog = InstructionMemory
  { _instructionCounter = initialInstructionCounter
  , _program            = prog
  }
