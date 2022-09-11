module HelVM.HelMA.Automata.Cat.Automaton (
  runWithParams,
  run
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams
import           HelVM.HelMA.Automaton.IO.BusinessIO

runWithParams :: BIO m => RunParams -> m ()
runWithParams = run . source

run :: BusinessIO m => Source -> m ()
run = wPutStr
