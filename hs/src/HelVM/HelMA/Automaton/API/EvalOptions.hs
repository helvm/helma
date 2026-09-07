module HelVM.HelMA.Automaton.API.EvalOptions where

import qualified HelVM.HelMA.Automaton.API.AutomatonOptions as Automaton
import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.MemoryOptions
import           HelVM.HelMA.Automaton.API.ParserOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Accessors
ramAutoOptions ∷ EvalOptions → RAMType
ramAutoOptions = ram . memoryOptions

stackAutoOptions ∷ EvalOptions → StackType
stackAutoOptions = stack . memoryOptions

cellAutoOptions ∷ EvalOptions → CellType
cellAutoOptions = cell . memoryOptions

intCellAutoOptions ∷ EvalOptions → IntCellType
intCellAutoOptions = intCell . memoryOptions

dumpAutoOptions ∷ EvalOptions → DumpType
dumpAutoOptions = dumpType . autoOptions

automatonOptions ∷ EvalOptions → Automaton.AutomatonOptions
automatonOptions o = Automaton.AutomatonOptions (stackAutoOptions o) (ramAutoOptions o) (autoOptions o)

-- | Type
data EvalOptions
  = EvalOptions
      { parserOptions :: !ParserOptions
      , memoryOptions :: !MemoryOptions
      , autoOptions   :: !AutoOptions
      }
