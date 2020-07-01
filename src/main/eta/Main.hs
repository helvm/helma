{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelCam.Common.Util
import HelVM.HelCam.Machines.BrainFuck.Evaluator
import HelVM.HelCam.Machines.WhiteSpace.Evaluator
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.Lexer

import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator.InteractEvaluator  as BFIE (interactEvalBF)
import qualified HelVM.HelCam.Machines.BrainFuck.Evaluator.MonadicEvaluator   as BFME (monadicEvalBF)
import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluator as WSIE (interactEvalWS)
import qualified HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluator  as WSME (monadicEvalWS)

import AppOptions

import Options.Applicative
import System.IO
import Text.Pretty.Simple

main :: IO ()
main = execParser opts >>= run where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelCam: The Interpreter of BrainFuck and WhiteSpace"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

run :: AppOptions -> IO ()
run AppOptions{lang, emitTL, emitIL, asciiLabels, etaMode, impl, file} = do
  hSetBuffering stdout NoBuffering
  source <- readFile file
  eval (computeLang lang) emitTL emitIL asciiLabels etaMode (computeImpl impl) source

eval :: Lang -> EmitTL -> EmitIL -> AsciiLabels -> EtaMode -> Impl -> Source -> IO ()
eval BF  _    _    _ True Interact = BFIE.interactEvalBF
eval BF  _    _    _ True _        = BFME.monadicEvalBF
eval BF  _    _    _ _    Interact = interactEvalBF
eval BF  _    _    _ _    _        = monadicEvalBF
eval WS  True _    _ _    _        = print . tokenizeWS
eval WS  _    True a _    _        = pPrintNoColor . parseWS a
eval WS  _    _    a True Interact = WSIE.interactEvalWS a
eval WS  _    _    a True _        = WSME.monadicEvalWS a
eval WS  _    _    a _    Interact = interactEvalWS a
eval WS  _    _    a _    _        = monadicEvalWS a
eval _   _    _    _ _    _        = putStrLn
