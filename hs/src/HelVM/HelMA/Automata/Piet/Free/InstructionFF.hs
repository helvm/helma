{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automata.Piet.Free.InstructionFF
  ( InstructionF (..)
  , InstructionFF
  , instructionF
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Instruction

import           Control.Monad.Free
import           Control.Monad.Free.TH                       ( makeFree )

type InstructionFF = Free InstructionF ()

data InstructionF a
  = InstructionF Instruction a
  deriving stock (Eq, Functor, Show)

makeFree ''InstructionF
