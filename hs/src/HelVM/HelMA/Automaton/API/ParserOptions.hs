module HelVM.HelMA.Automaton.API.ParserOptions where

import           HelVM.HelMA.Automaton.API.LabelType
import           HelVM.HelMA.Automaton.API.OptimizationLevel

simpleAutoParams ∷ LabelType → ParserOptions
simpleAutoParams l = ParserOptions
  { labelType = l
  , optLevel  = AllOptimizations
  }

-- | Type
data ParserOptions
  = ParserOptions
      { labelType :: !LabelType
      , optLevel  :: !OptimizationLevel
      }
