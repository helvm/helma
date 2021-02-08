{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelCam.Common.Util

import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator.InteractEvaluator   as BFIE   (interactEval)
import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator.MonadicEvaluator    as BFME   (monadicEval)
import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator                     as BF

import qualified HelVM.HelCam.Machines.ETA.Evaluator.InteractEvaluator         as ETAIE  (interactEval)
import qualified HelVM.HelCam.Machines.ETA.Evaluator.MonadicEvaluator          as ETAME  (monadicEval)
import qualified HelVM.HelCam.Machines.ETA.Evaluator                           as ETA
import qualified HelVM.HelCam.Machines.ETA.Lexer                               as ETA

import qualified HelVM.HelCam.Machines.SubLeq.Evaluator.InteractEvaluator      as SQIE   (interactEval)
import qualified HelVM.HelCam.Machines.SubLeq.Evaluator.MonadicEvaluator       as SQME   (monadicEval)
import qualified HelVM.HelCam.Machines.SubLeq.Evaluator                        as SQ
import qualified HelVM.HelCam.Machines.SubLeq.Lexer                            as SQ

import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluator  as WSIE   (interactEval)
import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluator   as WSME   (monadicEval)
import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator                    as WS
import qualified HelVM.HelCam.Machines.WhiteSpace.Parser                       as WS
import qualified HelVM.HelCam.Machines.WhiteSpace.Lexer                        as WS

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
run AppOptions{lang, emitTL, emitIL, asciiLabels, etaMode, impl, exec, file} = do
  IO.hSetBuffering stdout IO.NoBuffering
  source <- readFile2 exec file
  eval (computeLang lang) emitTL emitIL asciiLabels etaMode (computeImpl impl) source

readFile2 :: Exec -> String -> IO Source
readFile2 True file = return   file
readFile2 _    file = readFile file

eval :: Lang -> EmitTL -> EmitIL -> AsciiLabels -> EtaMode -> Impl -> Source -> IO ()
eval BF   _    _    _ True Interact = BFIE.interactEval
eval BF   _    _    _ True _        = BFME.monadicEval
eval BF   _    _    _ _    Interact = BF.interactEval
eval BF   _    _    _ _    _        = BF.monadicEval

eval ETA  True _    _ _    _        = print . ETA.tokenize
eval ETA  _    _    _ True Interact = ETAIE.interactEval
eval ETA  _    _    _ True _        = ETAME.monadicEval
eval ETA  _    _    _ _    Interact = ETA.interactEval
eval ETA  _    _    _ _    _        = ETA.monadicEval

eval SQ   True _    _ _    _        = print . SQ.tokenize
eval SQ   _    _    _ True Interact = SQIE.interactEval
eval SQ   _    _    _ True _        = SQME.monadicEval
eval SQ   _    _    _ _    Interact = SQ.interactEval
eval SQ   _    _    _ _    _        = SQ.monadicEval

eval WS   True _    _ _    _        = print . WS.tokenize
eval WS   _    True a _    _        = pPrintNoColor . WS.parse a
eval WS   _    _    a True Interact = WSIE.interactEval a
eval WS   _    _    a True _        = WSME.monadicEval a
eval WS   _    _    a _    Interact = WS.interactEval a
eval WS   _    _    a _    _        = WS.monadicEval a

eval _    _    _    _ _    _        = putStrLn
