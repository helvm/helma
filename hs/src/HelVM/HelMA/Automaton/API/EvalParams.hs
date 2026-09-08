module HelVM.HelMA.Automaton.API.EvalParams where

import           HelVM.HelMA.Automaton.API.EvalOptions
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.API.ParserOptions

toInstructionParams ∷ EvalParams → (OptimizationLevel, Source)
toInstructionParams p = (optLevel $ parserOptions (evalOptions p), source p)

-- | Type
data EvalParams
  = EvalParams
      { source      :: !Source
      , evalOptions :: !EvalOptions
      }
