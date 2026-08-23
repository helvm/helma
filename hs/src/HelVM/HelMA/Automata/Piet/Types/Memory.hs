module HelVM.HelMA.Automata.Piet.Types.Memory
  ( Memory (..)
  , Stack
  , blockCodelCount
  , codelSizeMemory
  , colourAt
  , currentPixel
  , directionPointerMemory
  , discoverBlock
  , handleCollision
  , initialMemory
  , instructionCounterMemory
  , instructionMemory
  , modifyFlipWithLog
  , modifyInstructionMemory
  , modifyStack
  , modifyStackWithLog
  , orientationMemory
  , positionMemory
  , programMemory
  , rotatePointer
  , selectCodel
  , setInstructionCounter
  , setPosition
  , stack
  , stepWhitePixel
  , toggleChooser
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

-- GETTERS

programMemory ∷ Memory → Program
programMemory mem = mem ^. instructionMemory . program

instructionCounterMemory ∷ Memory → InstructionCounter
instructionCounterMemory mem = mem ^. instructionMemory . instructionCounter

directionPointerMemory ∷ Memory → DirectionPointer
directionPointerMemory mem = orientationMemory mem ^. directionPointer

codelChooserMemory ∷ Memory → CodelChooser
codelChooserMemory mem = orientationMemory mem ^. codelChooser

positionMemory ∷ Memory → Coordinates
positionMemory mem = instructionCounterMemory mem ^. position

orientationMemory ∷ Memory → Orientation
orientationMemory mem = instructionCounterMemory mem ^. orientation

currentPixel ∷ Memory → Color
currentPixel mem = pixelImage (positionMemory mem) (programMemory mem ^. image)

-- SETTERS & FIELD MODIFIERS

setInstructionCounter ∷ InstructionCounter → Memory → Memory
setInstructionCounter ic = instructionMemory . instructionCounter .~ ic

setPosition ∷ Coordinates → Memory → Memory
setPosition pos = instructionMemory . instructionCounter . position .~ pos

setDirectionPointer ∷ DirectionPointer → Memory → Memory
setDirectionPointer dp = instructionMemory . instructionCounter . orientation . directionPointer .~ dp

setCodelChooser ∷ CodelChooser → Memory → Memory
setCodelChooser cc = instructionMemory . instructionCounter . orientation . codelChooser .~ cc

modifyInstructionMemory ∷ (InstructionMemory → InstructionMemory) → Memory → Memory
modifyInstructionMemory f = instructionMemory %~ f

modifyStack ∷ (Stack → Stack) → Memory → Memory
modifyStack f = stack %~ f

-- OPERATIONS & DOMAIN MODIFIERS

stepWhitePixel ∷ Memory → Memory
stepWhitePixel mem = handleBlocked (isBlocked nextPos prog) nextPos dp mem where
  prog    = programMemory mem
  nextPos = addCoordinates dp $ positionMemory mem
  dp      = directionPointerMemory mem

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

handleCollision ∷ Bool → Memory → Memory
handleCollision True  = toggleChooser
handleCollision False = rotatePointer

modifyStackWithLog ∷ AppSafeEff m ⇒ Text → (Stack → m Stack) → Memory → m Memory
modifyStackWithLog name f (Memory im s) = logWithPosition name im *> (Memory im <$> f s)

codelSizeMemory ∷ Memory → CodelSize
codelSizeMemory mem = programMemory mem ^. codelSize

blockCodelCount ∷ Block → Memory → Int
blockCodelCount block mem = olength block `div` (cs * cs) where
  cs = codelSizeMemory mem

selectCodel ∷ Block → Memory → Coordinates
selectCodel block mem = List.maximumBy (furthest (orientationMemory mem)) block

modifyFlipWithLog ∷ AppSafeEff m ⇒ Text → (Int → InstructionMemory → InstructionMemory) → Memory → m (Maybe Memory)
modifyFlipWithLog name f mem = case mem ^. stack of
  Seq.Empty     -> pure Nothing
  (x Seq.:<| _) -> do
    let mem' = modifyInstructionMemory (f x) mem
    logWithPosition (name <> " " <> show (directionPointerMemory mem')) (mem' ^. instructionMemory)
    pure $ Just mem'

-- Board and Color queries

colourAt ∷ Program → Coordinates → Maybe Color
colourAt prog pos = (prog ^. image) &! pos
