module HelVM.HelMA.Automata.LazyK.Automaton (
  run,
  runWithTerminator,
  realize,
  realizeWithTrue,
) where

import           HelVM.HelMA.Automata.LazyK.Constants
import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Reducer

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

run :: BIO m => Lambda -> m ()
run = runWithTerminator false

runWithTerminator :: BIO m => Lambda -> Lambda -> m ()
runWithTerminator terminator lambda = output terminator lambda =<< realizeWithTrue lambda

realizeWithTrue :: MonadSafe m => Lambda -> m Natural
realizeWithTrue = realize . flippedApply true

realize :: MonadSafe m => Lambda -> m Natural
realize = naturalSafe . flippedApply number0 . flippedApply Succ

number0 :: Lambda
number0 = Number 0

naturalSafe :: MonadSafe m => Lambda -> m Natural
naturalSafe (Number x) = pure x
naturalSafe x          = liftErrorWithPrefix "Invalid output format. Output should be the list of Church numerals. " $ show x

output :: BIO m => Lambda -> Lambda -> Natural -> m ()
output terminator lambda number = check $ compare 256 number where
  check GT = wPutAsChar number *> runWithTerminator terminator (apply lambda terminator)
  check EQ = pass
  check LT = wLogStr (show number) *> wLogStr (show lambda)
