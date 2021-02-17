{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelCam.Common.Types.CellType
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
run AppOptions{lang, emitTL, emitIL, asciiLabels, impl, ramType, cellType, exec, file} = do
  IO.hSetBuffering stdout IO.NoBuffering
  source <- readSource exec file
  run' emitTL emitIL (parseImpl impl) (parseRAMType ramType) (parseCellType cellType) asciiLabels (parseLang lang) source

readSource :: Exec -> String -> IO Source
readSource True = return
readSource _    = readFile

run' :: EmitTL -> EmitIL -> Impl -> RAMType -> CellType -> AsciiLabels -> Lang -> Source -> IO ()
run' True _    _ _ _ _ = tokenize
run' _    True _ _ _ a = parse a
run' _    _    i m c a = eval i m c a

tokenize :: Lang -> Source -> IO ()
tokenize ETA  = print . ETA.tokenize
tokenize SQ   = print . SQ.tokenize
tokenize WS   = print . WS.tokenize
tokenize _    = print

parse :: AsciiLabels -> Lang -> Source -> IO ()
parse a WS   s = pPrintNoColor $ WS.parse s a
parse _ lang s = tokenize lang s

eval :: Impl -> RAMType -> CellType -> AsciiLabels -> Lang -> Source -> IO ()
eval Interact m c a l s = IO.interact $ interactEval m c a l s
eval Monadic  m c a l s = monadicEval m c a l s

interactEval :: RAMType -> CellType -> AsciiLabels -> Lang -> Source -> (Input -> Output)
interactEval _ c _ BF  source = BF.eval source c
interactEval _ _ _ ETA source = ETA.eval source
interactEval m _ _ SQ  source = SQ.eval source m
interactEval m _ a WS  source = WS.eval source a m
interactEval _ _ _ Cat source = const source

monadicEval :: RAMType -> CellType -> AsciiLabels -> Lang -> Source -> IO ()
monadicEval _ c _ BF  source = BF.eval  source c
monadicEval _ _ _ ETA source = ETA.eval source
monadicEval m _ _ SQ  source = SQ.eval  source m
monadicEval m _ a WS  source = WS.eval  source a m
monadicEval _ _ _ Cat source = print source
