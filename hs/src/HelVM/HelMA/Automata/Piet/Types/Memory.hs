module HelVM.HelMA.Automata.Piet.Types.Memory (
  stepWhitePixel,
  programMemory,
  positionMemory,
  orientationMemory,
  initialMemory,
  setInstructionCounter,
  Memory (..),
  Stack,
) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Orientation
import           HelVM.HelMA.Automata.Piet.Types.Program

import qualified Data.Sequence                                      as Seq

import           Prelude                                            hiding (empty)

stepWhitePixel :: Memory -> Memory
stepWhitePixel mem = handleBlocked (isBlocked nextPos prog) nextPos dp mem where
  prog    = programMemory mem
  nextPos = addCoordinates dp $ positionMemory mem
  dp      = directionPointerMemory mem

-- INITIALIZERS & CONSTRUCTORS

initialMemory :: Program -> Memory
initialMemory prog = Memory
  { instructionMemory = initialInstructionMemory prog
  , stack             = Seq.empty
  }

-- GETTERS

programMemory :: Memory -> Program
programMemory = program . instructionMemory

instructionCounterMemory :: Memory -> InstructionCounter
instructionCounterMemory = instructionCounter . instructionMemory

directionPointerMemory :: Memory -> DirectionPointer
directionPointerMemory = directionPointer . orientationMemory

codelChooserMemory :: Memory -> CodelChooser
codelChooserMemory = codelChooser . orientationMemory

positionMemory :: Memory -> Coordinates
positionMemory = position . instructionCounterMemory

orientationMemory :: Memory -> Orientation
orientationMemory = orientation . instructionCounterMemory

-- SETTERS

setInstructionMemory :: InstructionMemory -> Memory -> Memory
setInstructionMemory im mem = mem { instructionMemory = im }

setInstructionCounter :: InstructionCounter -> Memory -> Memory
setInstructionCounter ic mem = setInstructionMemory ((instructionMemory mem) { instructionCounter = ic }) mem

setPosition :: Coordinates -> Memory -> Memory
setPosition pos mem = setInstructionCounter ((instructionCounterMemory mem) { position = pos }) mem

setDirectionPointer :: DirectionPointer -> Memory -> Memory
setDirectionPointer dp mem = setOrientation ((orientationMemory mem) { directionPointer = dp }) mem

setCodelChooser :: CodelChooser -> Memory -> Memory
setCodelChooser cc mem = setOrientation ((orientationMemory mem) { codelChooser = cc }) mem

setOrientation :: Orientation -> Memory -> Memory
setOrientation reg mem = setInstructionCounter ((instructionCounterMemory mem) { orientation = reg }) mem

-- OPERATIONS & MODIFIERS

handleBlocked :: Bool -> Coordinates -> DirectionPointer -> Memory -> Memory
handleBlocked True  _       dp = rotateAndToggle dp
handleBlocked False nextPos _  = setPosition nextPos

rotateAndToggle :: DirectionPointer -> Memory -> Memory
rotateAndToggle dp mem = setCodelChooser (toggle 1 (codelChooserMemory mem)) $ setDirectionPointer (rotate 1 dp) mem

-- DATA TYPES

data Memory = Memory
  { instructionMemory :: InstructionMemory
  , stack             :: !Stack
  }

type Stack = Seq Int
