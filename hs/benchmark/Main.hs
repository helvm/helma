module Main where

import           HelVM.HelMA.Automata.BrainFuck.EvaluatorBenchMark  as BF
import           HelVM.HelMA.Automata.ETA.EvaluatorBenchMark        as ETA
import           HelVM.HelMA.Automata.SubLeq.EvaluatorBenchMark     as SQ
import           HelVM.HelMA.Automata.WhiteSpace.EvaluatorBenchMark as WS

import           Gauge.Main
import           Gauge.Main.Options

main :: IO ()
main = defaultMainWith helConfig
  [ ETA.benchMark
  , WS.benchMark
  , BF.benchMark
  , SQ.benchMark
  ]

helConfig :: Config
helConfig = defaultConfig
  { resamples = 100
  }
