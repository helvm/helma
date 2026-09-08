module HelVM.HelMA.LangCommand where

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType   as BF

import           HelVM.HelMA.Automaton.API.AutomatonType

import           HelVM.HelMA.Automata.Piet.API.Options


import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType

data LangCommand
  = BFCommand !BF.ImplType
  | ETACommand !AutomatonType
  | FCommand
  | PietCommand !Options
  | SQCommand
  | WSCommand !TokenType
  | CatCommand
  | RevCommand
  | LazyCommand
  | ZotCommand
