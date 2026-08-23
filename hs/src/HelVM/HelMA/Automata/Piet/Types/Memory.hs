{-# LANGUAGE TemplateHaskell #-}
module HelVM.HelMA.Automata.Piet.Types.Memory
  ( Memory (..)
  , Stack
  , initialMemory
  , instructionMemory
  , orientationMemory
  , positionMemory
  , programMemory
  , setInstructionCounter
  , stack
  , stepWhitePixel
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Orientation
import           HelVM.HelMA.Automata.Piet.Types.Program

import qualified Data.Sequence                                      as Seq

import           Lens.Micro                                         ( (.~), (^.) )
import           Lens.Micro.TH                                      ( makeLenses )

import           Prelude                                            hiding ( empty )

data Memory
  = Memory
      { _instructionMemory :: !InstructionMemory
      , _stack             :: !Stack
      }

type Stack = Seq.Seq Int

makeLenses ''Memory

stepWhitePixel ∷ Memory → Memory
stepWhitePixel mem = handleBlocked (isBlocked nextPos prog) nextPos dp mem where
  prog    = programMemory mem
  nextPos = addCoordinates dp $ positionMemory mem
  dp      = directionPointerMemory mem

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

-- SETTERS

setInstructionCounter ∷ InstructionCounter → Memory → Memory
setInstructionCounter ic = instructionMemory . instructionCounter .~ ic

setPosition ∷ Coordinates → Memory → Memory
setPosition pos = instructionMemory . instructionCounter . position .~ pos

setDirectionPointer ∷ DirectionPointer → Memory → Memory
setDirectionPointer dp = instructionMemory . instructionCounter . orientation . directionPointer .~ dp

setCodelChooser ∷ CodelChooser → Memory → Memory
setCodelChooser cc = instructionMemory . instructionCounter . orientation . codelChooser .~ cc

-- OPERATIONS & MODIFIERS

handleBlocked ∷ Bool → Coordinates → DirectionPointer → Memory → Memory
handleBlocked True  _       dp = rotateAndToggle dp
handleBlocked False nextPos _  = setPosition nextPos

rotateAndToggle ∷ DirectionPointer → Memory → Memory
rotateAndToggle dp mem = setCodelChooser (toggle 1 (codelChooserMemory mem)) $ setDirectionPointer (rotate 1 dp) mem
