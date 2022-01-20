module HelVM.HelMA.Automaton.API.EvalParams where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Accessors
ramTypeOptions :: EvalParams -> RAMType
ramTypeOptions = ram . typeOptions

stackTypeOptions :: EvalParams -> StackType
stackTypeOptions = stack . typeOptions

cellTypeOptions :: EvalParams -> CellType
cellTypeOptions = cell . typeOptions

intCellTypeOptions :: EvalParams -> IntCellType
intCellTypeOptions = intCell . typeOptions

-- | Type
data EvalParams = EvalParams
  { compile     :: !Bool
  , asciiLabel  :: !Bool
  , source      :: !Source
  , typeOptions :: !TypeOptions
  }
