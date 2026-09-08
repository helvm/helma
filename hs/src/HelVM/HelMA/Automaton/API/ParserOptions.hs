module HelVM.HelMA.Automaton.API.ParserOptions where

import           HelVM.HelMA.Automaton.API.LabelType
import           HelVM.HelMA.Automaton.API.OptimizationLevel

simpleParserOptions ∷ ParserOptions
simpleParserOptions = ParserOptions
   AllOptimizations
   defaultLabelType

simpleParserOptionsWithLabel ∷ LabelType → ParserOptions
simpleParserOptionsWithLabel = ParserOptions AllOptimizations

-- | Type
data ParserOptions
  = ParserOptions
      { optLevel  :: !OptimizationLevel
      , labelType :: !LabelType
      }
