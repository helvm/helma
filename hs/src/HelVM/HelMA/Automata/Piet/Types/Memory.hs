module HelVM.HelMA.Automata.Piet.Types.Memory
  ( Memory (..)
  , Stack
  , advancePosition
  , blockCodelCount
  , codelSizeMemory
  , currentBlock
  , currentColour
  , currentPixel
  , directionPointerMemory
  , handleCollision
  , initialMemory
  , instructionCounterMemory
  , instructionMemory
  , modifyFlipWithLog
  , modifyStackWithLog
  , nextCodelPos
  , nextColour
  , orientationMemory
  , positionMemory
  , programMemory
  , selectCodel
  , setInstructionCounter
  , stack
  , stepWhitePixel
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Orientation
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff

import qualified Data.List                                          as List
import           Data.MonoTraversable
import qualified Data.Sequence                                      as Seq

import           Lens.Micro.Platform

import           Prelude                                            hiding ( empty )

-- TYPES & LENSES

data Memory
  = Memory
      { _instructionMemory :: !InstructionMemory
      , _stack             :: !Stack
      }

type Stack = Seq.Seq Int

makeLenses ''Memory

-- INITIALIZERS & CONSTRUCTORS

initialMemory ∷ Program → Memory
initialMemory prog = Memory
  { _instructionMemory = initialInstructionMemory prog
  , _stack             = Seq.empty
  }

-- PUBLIC GETTERS & QUERIES

nextCodelPos ∷ Memory → Coordinates
nextCodelPos mem = nextPosFromBlock (currentBlock mem) mem

nextPosFromBlock ∷ Block → Memory → Coordinates
nextPosFromBlock block mem = move (directionPointerMemory mem) (selectCodel block mem)

currentBlock ∷ Memory → Block
currentBlock mem = discoverBlock (programMemory mem ^. image) (positionMemory mem)

programMemory ∷ Memory → Program
programMemory mem = mem ^. instructionMemory . program

instructionCounterMemory ∷ Memory → InstructionCounter
instructionCounterMemory mem = mem ^. instructionMemory . instructionCounter

directionPointerMemory ∷ Memory → DirectionPointer
directionPointerMemory mem = orientationMemory mem ^. directionPointer

positionMemory ∷ Memory → Coordinates
positionMemory mem = instructionCounterMemory mem ^. position

orientationMemory ∷ Memory → Orientation
orientationMemory mem = instructionCounterMemory mem ^. orientation

currentPixel ∷ Memory → Color
currentPixel mem = pixelImage (positionMemory mem) (programMemory mem ^. image)

codelSizeMemory ∷ Memory → CodelSize
codelSizeMemory mem = programMemory mem ^. codelSize

blockCodelCount ∷ Memory → Int
blockCodelCount mem = olength (currentBlock mem) `div` (cs * cs) where
  cs = codelSizeMemory mem

selectCodel ∷ Block → Memory → Coordinates
selectCodel block mem = List.maximumBy (furthest (orientationMemory mem)) block

currentColour ∷ Memory → Maybe Color
currentColour mem = colourAt (programMemory mem) (positionMemory mem)

nextColour ∷ Memory → Maybe Color
nextColour mem = colourAt (programMemory mem) (nextCodelPos mem)

colourAt ∷ Program → Coordinates → Maybe Color
colourAt prog pos = (prog ^. image) &! pos

-- PUBLIC SETTERS

advancePosition ∷ Memory → Memory
advancePosition mem = setPosition (nextCodelPos mem) mem

setInstructionCounter ∷ InstructionCounter → Memory → Memory
setInstructionCounter ic = instructionMemory . instructionCounter .~ ic

setPosition ∷ Coordinates → Memory → Memory
setPosition pos = instructionMemory . instructionCounter . position .~ pos

-- PUBLIC DOMAIN MODIFIERS

stepWhitePixel ∷ Memory → Memory
stepWhitePixel mem = handleBlocked (isBlocked nextPos prog) nextPos dp mem where
  prog    = programMemory mem
  nextPos = addCoordinates dp $ positionMemory mem
  dp      = directionPointerMemory mem

handleCollision ∷ Bool → Memory → Memory
handleCollision False = rotatePointer
handleCollision True  = toggleChooser

-- PUBLIC MONADIC EFFECT MODIFIERS

modifyStackWithLog ∷ AppSafeEff m ⇒ Text → (Stack → m Stack) → Memory → m Memory
modifyStackWithLog name f (Memory im s) = logWithPosition name im *> (Memory im <$> f s)

modifyFlipWithLog ∷ AppSafeEff m ⇒ Text → (Int → InstructionMemory → InstructionMemory) → Memory → m (Maybe Memory)
modifyFlipWithLog name f mem = case mem ^. stack of
  Seq.Empty     -> pure Nothing
  (x Seq.:<| _) -> do
    let mem' = modifyInstructionMemory (f x) mem
    logWithPosition (name <> " " <> show (directionPointerMemory mem')) (mem' ^. instructionMemory)
    pure $ Just mem'

-- PRIVATE UTILS & SETTERS

codelChooserMemory ∷ Memory → CodelChooser
codelChooserMemory mem = orientationMemory mem ^. codelChooser

setDirectionPointer ∷ DirectionPointer → Memory → Memory
setDirectionPointer dp = instructionMemory . instructionCounter . orientation . directionPointer .~ dp

setCodelChooser ∷ CodelChooser → Memory → Memory
setCodelChooser cc = instructionMemory . instructionCounter . orientation . codelChooser .~ cc

modifyInstructionMemory ∷ (InstructionMemory → InstructionMemory) → Memory → Memory
modifyInstructionMemory f = instructionMemory %~ f

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

