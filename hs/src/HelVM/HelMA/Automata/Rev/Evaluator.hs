module HelVM.HelMA.Automata.Rev.Evaluator (
  batchEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Util

import qualified Data.String as S

batchEval :: Source -> Output
batchEval = flip eval emptyInput

evalParams :: Evaluator r => EvalParams ->  r
evalParams = eval . source

eval :: Evaluator r => Source ->  r
eval = evalLines . S.lines

evalLines :: Evaluator r => [Source] -> r
evalLines ll = doOutput $ S.unlines $ reverse <$> ll

----

class Evaluator r where
  doOutput :: Source ->  r

----

instance Evaluator Interact where
  doOutput = const

----

instance WrapperIO m => Evaluator (m ()) where
  doOutput = wPutStr
