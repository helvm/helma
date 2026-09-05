module HelVM.HelMA.Automata.ETA.Parser
  ( parse
  ) where

import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer


import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.OptimizationLevel

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelIO.Control.Safe

parse ∷ MonadSafe m ⇒ OptimizationLevel → Source → m InstructionList
parse ol = optimize ol . tokenize
