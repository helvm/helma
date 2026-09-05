module HelVM.HelMA.Automaton.API.ParserOptions where

import           HelVM.HelMA.Automaton.API.LabelType
import           HelVM.HelMA.Automaton.API.OptimizationLevel

simpleAutoParams ∷ LabelType → ParserOptions
simpleAutoParams l = ParserOptions
  { optLevel  = AllOptimizations
  ,  labelType = l
  }

-- | Type
data ParserOptions
  = ParserOptions
      { optLevel  :: !OptimizationLevel
      , labelType :: !LabelType
      }
