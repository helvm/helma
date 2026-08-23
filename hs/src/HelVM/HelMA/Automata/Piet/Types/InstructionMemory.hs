module HelVM.HelMA.Automata.Piet.Types.InstructionMemory
  ( InstructionMemory (..)
  , codelChooserIM
  , directionPointerIM
  , getX
  , getY
  , initialInstructionMemory
  , instructionCounter
  , logWithPosition
  , nonBlackSucc
  , program
  , rotateDirectionPointerIM
  , succCoordinates
  , toggleCodelChooserIM
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Orientation
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import           Lens.Micro.Platform

data InstructionMemory
  = InstructionMemory
      { _instructionCounter :: !InstructionCounter
      , _program            :: !Program
      }

makeLenses ''InstructionMemory

initialInstructionMemory ∷ Program → InstructionMemory
initialInstructionMemory prog = InstructionMemory
  { _instructionCounter = initialInstructionCounter
  , _program            = prog
  }


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


nonBlackSucc ∷ Program → Maybe LabelInfo → Orientation → Maybe InstructionCounter
nonBlackSucc prog mStats reg = uncurry InstructionCounter <$> find isValid (zip (fmap (succCoordinates mStats) directions) directions) where
  directions       = flip rotateToggle reg <$> zip [ 0, 0, 1, 1, 2, 2, 3, 3 ] (0 : cycle [ 1, 1, 0, 0 ])
  isValid (pos, _) = not (isBlocked pos prog)

succCoordinates ∷ Maybe LabelInfo → Orientation → Coordinates
succCoordinates labelInfo reg = addCoordinates (reg ^. directionPointer) $ toCooCoordinates labelInfo reg

toCooCoordinates ∷ Maybe LabelInfo → Orientation → Coordinates
toCooCoordinates (Just labelInfo) reg = (getX reg labelInfo, getY reg labelInfo)
toCooCoordinates Nothing          _   = (0, 0)

getX ∷ Orientation → LabelInfo → Int
getX (Orientation DPRight CCLeft)  lblInfo = lblInfo ^. labelRight . borderCoord
getX (Orientation DPRight CCRight) lblInfo = lblInfo ^. labelRight . borderCoord
getX (Orientation DPDown  CCLeft)  lblInfo = lblInfo ^. labelBottom . borderMax
getX (Orientation DPDown  CCRight) lblInfo = lblInfo ^. labelBottom . borderMin
getX (Orientation DPLeft  CCLeft)  lblInfo = lblInfo ^. labelLeft . borderCoord
getX (Orientation DPLeft  CCRight) lblInfo = lblInfo ^. labelLeft . borderCoord
getX (Orientation DPUp    CCLeft)  lblInfo = lblInfo ^. labelTop . borderMin
getX (Orientation DPUp    CCRight) lblInfo = lblInfo ^. labelTop . borderMax

getY ∷ Orientation → LabelInfo → Int
getY (Orientation DPRight CCLeft)  lblInfo = lblInfo ^. labelRight . borderMin
getY (Orientation DPRight CCRight) lblInfo = lblInfo ^. labelRight . borderMax
getY (Orientation DPDown  CCLeft)  lblInfo = lblInfo ^. labelBottom . borderCoord
getY (Orientation DPDown  CCRight) lblInfo = lblInfo ^. labelBottom . borderCoord
getY (Orientation DPLeft  CCLeft)  lblInfo = lblInfo ^. labelLeft . borderMax
getY (Orientation DPLeft  CCRight) lblInfo = lblInfo ^. labelLeft . borderMin
getY (Orientation DPUp    CCLeft)  lblInfo = lblInfo ^. labelTop . borderCoord
getY (Orientation DPUp    CCRight) lblInfo = lblInfo ^. labelTop . borderCoord
