module HelVM.HelMA.LangCommand where

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType   as BF

import           HelVM.HelMA.Automaton.API.AutomatonType

import           HelVM.HelMA.Automata.Piet.API.PietOptions


import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType

data LangCommand
  = BFCommand !BF.ImplType
  | ETACommand !AutomatonType
  | FCommand
  | PietCommand !PietOptions
  | SQCommand
  | WSCommand !TokenType
  | CatCommand
  | RevCommand
  | LazyCommand
  | ZotCommand
