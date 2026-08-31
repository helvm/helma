module HelVM.HelMA.Automata.Piet.Types.InstructionMemory
  ( InstructionMemory (..)
  , codelChooserIM
  , cursorL
  , directionPointerIM
  , initialInstructionMemory
  , logWithPosition
  , nonBlackSucc
  , programL
  , rotateDirectionPointerIM
  , succCoordinates
  , toggleCodelChooserIM
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import           Relude.Extra

-- TYPES & LENSES

data InstructionMemory
  = InstructionMemory
      { cursor  :: !Cursor
      , program :: !Program
      }

cursorL ∷ Lens' InstructionMemory Cursor
cursorL = lens cursor (\s x -> s { cursor = x })

programL ∷ Lens' InstructionMemory Program
programL = lens program (\s x -> s { program = x })

-- INITIALIZATION & LOGGING

initialInstructionMemory ∷ Program → InstructionMemory
initialInstructionMemory prog = InstructionMemory
  { cursor = initialCursor
  , program            = prog
  }

logWithPosition ∷ AppSafeEff m ⇒ Text → InstructionMemory → m ()
logWithPosition msg im = logDebugN $ show (im ^. (cursorL . positionL)) <> " " <> msg

-- HELPER FUNCTIONS

codelChooserIM ∷ InstructionMemory → CodelChooser
codelChooserIM im = codelChooserIC (im ^. cursorL)

directionPointerIM ∷ InstructionMemory → DirectionPointer
directionPointerIM im = directionPointerIC (im ^. cursorL)

toggleCodelChooserIM ∷ Int → InstructionMemory → InstructionMemory
toggleCodelChooserIM n = cursorL %~ toggleCodelChooserIC n

rotateDirectionPointerIM ∷ Int → InstructionMemory → InstructionMemory
rotateDirectionPointerIM n = cursorL %~ rotateDirectionPointerIC n

-- SUCCESSOR & COORDINATES CALCULATIONS

nonBlackSucc ∷ Program → Course → Maybe LabelInfo → Maybe Cursor
nonBlackSucc prog reg mStats = uncurry Cursor <$> find isValid (zip (fmap (succCoordinates mStats) directions) directions) where
  directions       = flip rotateToggle reg <$> zip [ 0, 0, 1, 1, 2, 2, 3, 3 ] (0 : cycle [ 1, 1, 0, 0 ])
  isValid (pos, _) = not (isBlocked pos prog)

succCoordinates ∷ Maybe LabelInfo → Course → Coordinates
succCoordinates labelInfo reg = move (reg ^. directionPointerL) $ toCooCoordinates labelInfo reg

toCooCoordinates ∷ Maybe LabelInfo → Course → Coordinates
toCooCoordinates (Just labelInfo) reg = (getX reg labelInfo, getY reg labelInfo)
toCooCoordinates Nothing          _   = (0, 0)

getX ∷ Course → LabelInfo → Int
getX (Course DPRight CCLeft)  lblInfo = lblInfo ^. (labelRightL . borderCoordL)
getX (Course DPRight CCRight) lblInfo = lblInfo ^. (labelRightL . borderCoordL)
getX (Course DPDown  CCLeft)  lblInfo = lblInfo ^. (labelBottomL . borderMaxL)
getX (Course DPDown  CCRight) lblInfo = lblInfo ^. (labelBottomL . borderMinL)
getX (Course DPLeft  CCLeft)  lblInfo = lblInfo ^. (labelLeftL . borderCoordL)
getX (Course DPLeft  CCRight) lblInfo = lblInfo ^. (labelLeftL . borderCoordL)
getX (Course DPUp    CCLeft)  lblInfo = lblInfo ^. (labelTopL . borderMinL)
getX (Course DPUp    CCRight) lblInfo = lblInfo ^. (labelTopL . borderMaxL)

getY ∷ Course → LabelInfo → Int
getY (Course DPRight CCLeft)  lblInfo = lblInfo ^. (labelRightL . borderMinL)
getY (Course DPRight CCRight) lblInfo = lblInfo ^. (labelRightL . borderMaxL)
getY (Course DPDown  CCLeft)  lblInfo = lblInfo ^. (labelBottomL . borderCoordL)
getY (Course DPDown  CCRight) lblInfo = lblInfo ^. (labelBottomL . borderCoordL)
getY (Course DPLeft  CCLeft)  lblInfo = lblInfo ^. (labelLeftL . borderMaxL)
getY (Course DPLeft  CCRight) lblInfo = lblInfo ^. (labelLeftL . borderMinL)
getY (Course DPUp    CCLeft)  lblInfo = lblInfo ^. (labelTopL . borderCoordL)
getY (Course DPUp    CCRight) lblInfo = lblInfo ^. (labelTopL . borderCoordL)
