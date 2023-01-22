module HelVM.HelMA.Automaton.API.AutoOptionsExtra where

import           HelVM.HelMA.Automaton.API.AutoOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

defaultAutoOptions :: AutoOptions
defaultAutoOptions = AutoOptions
  { ram      = defaultRAMType
  , stack    = defaultStackType
  , cell     = defaultCellType
  , intCell  = defaultIntCellType
  }
