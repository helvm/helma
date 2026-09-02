module HelVM.HelMA.Automata.Piet.Types.Memory
  ( Memory (..)
  , Stack
  , advancePosition
  , blockCodelCount
  , currentColour
  , currentPixel
  , getMaskInfo
  , handleCollision
  , initialMemory
  , modifyStackWithLog
  , nextColour
  , nonBlackSuccMemory
  , setCursor
  , stackL
  , stepWhitePixel
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Labelling
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Data.IntMap                                       hiding ( filter )
import qualified Data.List                                         as List
import           Data.MonoTraversable
import qualified Data.Sequence                                     as Seq

import           Relude.Extra

-- TYPES & LENSES

type Stack = Seq.Seq Int

data Memory
  = Memory
      { instructionMemory :: !InstructionMemory
      , stack             :: !Stack
      }

instructionMemoryL ∷ Lens' Memory InstructionMemory
instructionMemoryL = lens instructionMemory (\s x -> s { instructionMemory = x })

stackL ∷ Lens' Memory Stack
stackL = lens stack (\s x -> s { stack = x })

-- INITIALIZERS & CONSTRUCTORS

initialMemory ∷ Program → Memory
initialMemory prog = Memory
  { instructionMemory = initialInstructionMemory prog
  , stack             = Seq.empty
  }

-- PUBLIC GETTERS & QUERIES

nonBlackSuccMemory ∷ Memory → Maybe LabelInfo → Maybe Cursor
nonBlackSuccMemory mem = nonBlackSucc (programMemory mem) (courseMemory mem)

getMaskInfo ∷ Memory → Maybe LabelInfo
getMaskInfo mem = getMaskInfo' (programMemory mem) (positionMemory mem)

nextCodelPos ∷ Memory → Coordinates
nextCodelPos mem = nextPosFromBlock (currentBlock mem) mem

currentBlock ∷ Memory → BlockCoordinates
currentBlock mem = discoverBlock (programMemory mem ^. imageL) (positionMemory mem)

programMemory ∷ Memory → Program
programMemory mem = mem ^. (instructionMemoryL . programL)

cursorMemory ∷ Memory → Cursor
cursorMemory mem = mem ^. (instructionMemoryL . cursorL)

directionPointerMemory ∷ Memory → DirectionPointer
directionPointerMemory mem = courseMemory mem ^. directionPointerL

positionMemory ∷ Memory → Coordinates
positionMemory mem = cursorMemory mem ^. positionL

courseMemory ∷ Memory → Course
courseMemory mem = cursorMemory mem ^. courseL

currentPixel ∷ Memory → Color
currentPixel mem = atGrid (positionMemory mem) (programMemory mem ^. imageL)

blockCodelCount ∷ Memory → Int
blockCodelCount = olength . currentBlock

selectCodel ∷ BlockCoordinates → Memory → Coordinates
selectCodel block mem = List.maximumBy (furthest (courseMemory mem)) block

currentColour ∷ Memory → Maybe Color
currentColour mem = colorAt (programMemory mem) (positionMemory mem)

nextColour ∷ Memory → Maybe Color
nextColour mem = colorAt (programMemory mem) (nextCodelPos mem)

-- PUBLIC SETTERS

advancePosition ∷ Memory → Memory
advancePosition mem = setPosition (nextCodelPos mem) mem

setCursor ∷ Cursor → Memory → Memory
setCursor ic = (instructionMemoryL . cursorL) .~ ic

-- PUBLIC DOMAIN MODIFIERS

stepWhitePixel ∷ Memory → Memory
stepWhitePixel mem = handleBlocked (isBlocked nextPos prog) nextPos dp mem
  where
    prog    = programMemory mem
    nextPos = move dp $ positionMemory mem
    dp      = directionPointerMemory mem

handleCollision ∷ Bool → Memory → Memory
handleCollision False = rotatePointer
handleCollision True  = toggleChooser

-- PUBLIC MONADIC EFFECT MODIFIERS

modifyStackWithLog ∷ AppSafeEff m ⇒ Text → (Stack → m Stack) → Memory → m Memory
modifyStackWithLog name f (Memory im s) = logWithPosition name im *> (Memory im <$> f s)

-- PRIVATE UTILS & HELPERS

getMaskInfo' ∷ Program → Coordinates → Maybe LabelInfo
getMaskInfo' prog pos = findWithDefault Nothing (atGrid pos maskImg) infoMap
  where
    maskImg = prog ^. (labellingL . maskL)
    infoMap = prog ^. (labellingL . infoL)

nextPosFromBlock ∷ BlockCoordinates → Memory → Coordinates
nextPosFromBlock block mem = move (directionPointerMemory mem) (selectCodel block mem)

colorAt ∷ Program → Coordinates → Maybe Color
colorAt prog pos = (prog ^. imageL) &! pos

setPosition ∷ Coordinates → Memory → Memory
setPosition pos = (instructionMemoryL . cursorL . positionL) .~ pos

handleBlocked ∷ Bool → Coordinates → DirectionPointer → Memory → Memory
handleBlocked True  _       dp = rotateAndToggle dp
handleBlocked False nextPos _  = setPosition nextPos

rotateAndToggle ∷ DirectionPointer → Memory → Memory
rotateAndToggle dp mem = mem
  & setCodelChooser (toggle 1 $ codelChooserMemory mem)
  & setDirectionPointer (rotate 1 dp)

toggleChooser ∷ Memory → Memory
toggleChooser = modifyInstructionMemory (toggleCodelChooserIM 1)

rotatePointer ∷ Memory → Memory
rotatePointer = modifyInstructionMemory (rotateDirectionPointerIM 1)

codelChooserMemory ∷ Memory → CodelChooser
codelChooserMemory mem = courseMemory mem ^. codelChooserL

setDirectionPointer ∷ DirectionPointer → Memory → Memory
setDirectionPointer dp = (instructionMemoryL . cursorL . courseL . directionPointerL) .~ dp

setCodelChooser ∷ CodelChooser → Memory → Memory
setCodelChooser cc = (instructionMemoryL . cursorL . courseL . codelChooserL) .~ cc

modifyInstructionMemory ∷ (InstructionMemory → InstructionMemory) → Memory → Memory
modifyInstructionMemory f = instructionMemoryL %~ f
