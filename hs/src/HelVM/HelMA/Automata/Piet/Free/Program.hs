{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module HelVM.HelMA.Automata.Piet.Free.Program
  ( InstructionF (..)
  , Program
  , instructionF
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Instruction

import           Control.Monad.Free
import           Control.Monad.Free.TH                       ( makeFree )

type Program = Free InstructionF ()

data InstructionF a
  = InstructionF Instruction a
  deriving stock (Eq, Functor, Show)

makeFree ''InstructionF
