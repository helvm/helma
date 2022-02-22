module Main where

import           HelVM.HelMA.Automata.BrainFuck.AutomatonBenchMark  as BF
import           HelVM.HelMA.Automata.ETA.AutomatonBenchMark        as ETA
import           HelVM.HelMA.Automata.SubLeq.AutomatonBenchMark     as SQ
import           HelVM.HelMA.Automata.WhiteSpace.AutomatonBenchMark as WS

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
