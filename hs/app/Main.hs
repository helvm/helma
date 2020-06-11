module Main where

import AppOptions

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.API.TypeOptions

import HelVM.HelMA.Common.Types.CellType
import HelVM.HelMA.Common.Types.StackType
import HelVM.HelMA.Common.Types.TokenType
import HelVM.HelMA.Common.Types.RAMType

import HelVM.HelMA.Common.Util

import qualified HelVM.HelMA.Automata.Cat.Evaluator        as Cat

import qualified HelVM.HelMA.Automata.Rev.Evaluator        as Rev

import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator  as BF
import qualified HelVM.HelMA.Automata.BrainFuck.Lexer      as BF

import qualified HelVM.HelMA.Automata.ETA.Evaluator        as ETA
import qualified HelVM.HelMA.Automata.ETA.Lexer            as ETA

import qualified HelVM.HelMA.Automata.SubLeq.Evaluator     as SQ
import qualified HelVM.HelMA.Automata.SubLeq.Lexer         as SQ

import qualified HelVM.HelMA.Automata.WhiteSpace.Evaluator as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Parser    as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Lexer     as WS

import Options.Applicative
import Text.Pretty.Simple

import qualified System.IO as IO

main :: IO ()
main = runApp =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelMA: The Interpreter of BrainFuck, ETA, SubLeq and WhiteSpace"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

runApp:: AppOptions -> IO ()
runApp AppOptions{lang, minified, emitTL, emitIL, asciiLabels, impl, ramType, stackType, cellType, exec, file} = do
  IO.hSetBuffering stdout IO.NoBuffering
  source <- readSource exec file
  run minified emitTL emitIL (parseImpl impl) (TypeOptions (parseRAMType ramType) (parseStackType stackType) (parseCellType cellType)) asciiLabels (parseLang lang) source

readSource :: Exec -> String -> IO Source
readSource True = pure
readSource _    = readFile

run :: Minified -> EmitTL -> EmitIL -> Impl -> TypeOptions -> AsciiLabels -> Lang -> Source -> IO ()
run True _    _    _ _ _ = minification
run _    True _    _ _ _ = tokenize
run _    _    True _ _ a = flip parse a
run _    _    _    i e a = eval i e a

minification :: Lang -> Source -> IO ()
minification BF   = print . BF.readTokens
minification ETA  = print . ETA.readTokens
minification SQ   = print . SQ.readSymbols
minification STN  = print . WS.readVisibleTokens
minification WS   = print . WS.readWhiteTokens
minification _    = print

tokenize :: Lang -> Source -> IO ()
tokenize ETA  = print . ETA.tokenize
tokenize SQ   = print . SQ.tokenize
tokenize STN  = print . WS.tokenizeVisible
tokenize WS   = print . WS.tokenizeWhite
tokenize _    = print

parse :: Lang -> AsciiLabels -> Source -> IO ()
parse STN  a = pPrintNoColor . flip (WS.parse VisibleTokenType)  a
parse WS   a = pPrintNoColor . flip (WS.parse WhiteTokenType) a
parse lang _ = tokenize lang

eval :: Impl -> TypeOptions -> AsciiLabels -> Lang -> Source -> IO ()
eval impl options a lang s = evalParams impl (lang, EvalParams {asciiLabel = a, source = s, typeOptions = options})

evalParams :: Impl -> (Lang, EvalParams) -> IO ()
evalParams Interact = IO.interact . interactEval'
evalParams Monadic  = monadicEval'

interactEval' :: (Lang, EvalParams) -> Interact
interactEval' (lang, params) = interactEval lang params

monadicEval' :: WrapperIO m => (Lang, EvalParams) -> m ()
monadicEval' (lang, params) = monadicEval lang params

interactEval :: Lang -> EvalParams -> Interact
interactEval Cat = Cat.evalParams
interactEval Rev = Rev.evalParams
interactEval BF  = BF.evalParams
interactEval ETA = ETA.evalParams
interactEval SQ  = SQ.evalParams
interactEval STN = WS.evalParams VisibleTokenType
interactEval WS  = WS.evalParams WhiteTokenType

monadicEval :: WrapperIO m => Lang -> EvalParams -> m ()
monadicEval Cat = Cat.evalParams
monadicEval Rev = Rev.evalParams
monadicEval BF  = BF.evalParams
monadicEval ETA = ETA.evalParams
monadicEval SQ  = SQ.evalParams
monadicEval STN = WS.evalParams VisibleTokenType
monadicEval WS  = WS.evalParams WhiteTokenType
