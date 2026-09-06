module HelVM.HelMA.Automaton.API.EvalParams where

import qualified HelVM.HelMA.Automaton.API.AutomatonOptions  as Automaton
import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.MemoryOptions
import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.API.ParserOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Accessors
ramAutoOptions ∷ EvalParams → RAMType
ramAutoOptions = ram . memoryOptions

stackAutoOptions ∷ EvalParams → StackType
stackAutoOptions = stack . memoryOptions

cellAutoOptions ∷ EvalParams → CellType
cellAutoOptions = cell . memoryOptions

intCellAutoOptions ∷ EvalParams → IntCellType
intCellAutoOptions = intCell . memoryOptions

dumpAutoOptions ∷ EvalParams → DumpType
dumpAutoOptions = dumpType . autoOptions

automatonOptions ∷ EvalParams → Automaton.AutomatonOptions
automatonOptions p = Automaton.AutomatonOptions (stackAutoOptions p) (ramAutoOptions p) (autoOptions p)

toInstructionParams ∷ EvalParams → (OptimizationLevel, Source)
toInstructionParams p = (optLevel (parserOptions p), source p)

-- | Type
data EvalParams
  = EvalParams
      { source        :: !Source
      , parserOptions :: !ParserOptions
      , memoryOptions :: !MemoryOptions
      , autoOptions   :: !AutoOptions
      }
