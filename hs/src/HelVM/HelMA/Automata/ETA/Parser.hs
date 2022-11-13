module HelVM.HelMA.Automata.ETA.Parser (
  parseSafe,
  parse,
) where

import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelIO.Control.Safe

parseSafe :: Source -> Safe InstructionList
parseSafe = parse

parse :: MonadSafe m => Source -> m InstructionList
parse = optimize . tokenize
