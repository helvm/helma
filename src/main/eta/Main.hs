{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelCam.Common.Types.CellType
import HelVM.HelCam.Common.Types.StackType
import HelVM.HelCam.Common.Types.RAMType
import HelVM.HelCam.Common.Util

import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator  as BF

import qualified HelVM.HelCam.Machines.ETA.Evaluator        as ETA
import qualified HelVM.HelCam.Machines.ETA.Lexer            as ETA

import qualified HelVM.HelCam.Machines.SubLeq.Evaluator     as SQ
import qualified HelVM.HelCam.Machines.SubLeq.Lexer         as SQ

import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator as WS
import qualified HelVM.HelCam.Machines.WhiteSpace.Parser    as WS
import qualified HelVM.HelCam.Machines.WhiteSpace.Lexer     as WS

import AppOptions
import EvalOptions

import Options.Applicative
import Text.Pretty.Simple

import qualified System.IO as IO

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelCam: The Interpreter of BrainFuck, ETA, SubLeq and WhiteSpace"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

run :: AppOptions -> IO ()
run AppOptions{lang, emitTL, emitIL, asciiLabels, impl, ramType, stackType, cellType, exec, file} = do
  IO.hSetBuffering stdout IO.NoBuffering
  source <- readSource exec file
  run' emitTL emitIL (parseImpl impl) (EvalOptions (parseRAMType ramType) (parseStackType stackType) (parseCellType cellType)) asciiLabels (parseLang lang) source

readSource :: Exec -> String -> IO Source
readSource True = return
readSource _    = readFile

run' :: EmitTL -> EmitIL -> Impl -> EvalOptions -> AsciiLabels -> Lang -> Source -> IO ()
run' True _    _ _ _ = tokenize
run' _    True _ _ a = parse a
run' _    _    i e a = eval i e a

tokenize :: Lang -> Source -> IO ()
tokenize ETA  = print . ETA.tokenize
tokenize SQ   = print . SQ.tokenize
tokenize WS   = print . WS.tokenize
tokenize _    = print

parse :: AsciiLabels -> Lang -> Source -> IO ()
parse a WS   s = pPrintNoColor $ WS.parse s a
parse _ lang s = tokenize lang s

eval :: Impl -> EvalOptions -> AsciiLabels -> Lang -> Source -> IO ()
eval Interact e a l s = IO.interact $ interactEval e a l s
eval Monadic  e a l s = monadicEval e a l s

interactEval :: EvalOptions -> AsciiLabels -> Lang -> Source -> (Input -> Output)
interactEval e _ BF  source = BF.eval  source (cell e)
interactEval e _ ETA source = ETA.eval source (stack e)
interactEval e _ SQ  source = SQ.eval  source (ram e)
interactEval e a WS  source = WS.eval  source a (stack e) (ram e)
interactEval _ _ Cat source = const    source

monadicEval :: EvalOptions -> AsciiLabels -> Lang -> Source -> IO ()
monadicEval e _ BF  source = BF.eval  source (cell e)
monadicEval e _ ETA source = ETA.eval source (stack e)
monadicEval e _ SQ  source = SQ.eval  source (ram e)
monadicEval e a WS  source = WS.eval  source a (stack e) (ram e)
monadicEval _ _ Cat source = print    source
