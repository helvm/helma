module Main where

import           AppOptions

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.Control.Control

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import qualified HelVM.HelMA.Automata.Cat.Evaluator        as Cat

import qualified HelVM.HelMA.Automata.Rev.Evaluator        as Rev

import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator  as BF
import qualified HelVM.HelMA.Automata.BrainFuck.Lexer      as BF

import qualified HelVM.HelMA.Automata.ETA.Evaluator        as ETA
import qualified HelVM.HelMA.Automata.ETA.Lexer            as ETA

import qualified HelVM.HelMA.Automata.SubLeq.Evaluator     as SQ
import qualified HelVM.HelMA.Automata.SubLeq.Lexer         as SQ

import qualified HelVM.HelMA.Automata.WhiteSpace.Evaluator as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Lexer     as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Parser    as WS

import           Options.Applicative
import           Text.Pretty.Simple

import qualified System.IO                                 as IO

main :: IO ()
main = runApp =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelMA: The Interpreter of BrainFuck , ETA , SubLeq and WhiteSpace"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

runApp:: AppOptions -> IO ()
--runApp AppOptions{lang , minified , emitTL , emitIL , printLogs , compile , asciiLabels , ramType , stackType , cellType , intCellType , exec , file} = do
runApp (AppOptions lang minified emitTL emitIL printLogs compile asciiLabels ramType stackType cellType intCellType exec file) = do
  hSetBuffering stdout IO.NoBuffering
  source <- readSource exec file
  run minified emitTL emitIL printLogs typeOptions asciiLabels compile (parseLang lang) source
    where typeOptions = TypeOptions (parseRAMType ramType) (parseStackType stackType) (parseCellType cellType) (parseIntCellType intCellType)

readSource :: Exec -> String -> IO Source
readSource True = pure . toText
readSource _    = readFileText

run :: Minified -> EmitTL -> EmitIL -> PrintLogs -> TypeOptions -> Compile -> AsciiLabels -> Lang -> Source -> IO ()
run True _    _    _ _ _ _ = minification
run _    True _    _ _ _ _ = tokenize
run _    _    True _ _ _ a = flip parse a
run _    _    _    p t c a = eval p t c a

minification :: Lang -> Source -> IO ()
minification BF  = print . BF.readTokens
minification ETA = print . ETA.readTokens
minification SQ  = print . SQ.readSymbols
minification STN = print . WS.readVisibleTokens
minification WS  = print . WS.readWhiteTokens
minification _   = print

tokenize :: Lang -> Source -> IO ()
tokenize ETA = print . ETA.tokenize
tokenize SQ  = print . SQ.tokenize
tokenize STN = print . WS.tokenizeVisible
tokenize WS  = print . WS.tokenizeWhite
tokenize _   = print

parse :: Lang -> AsciiLabels -> Source -> IO ()
parse STN  a = pPrintNoColor . WS.flipParseVisible a
parse WS   a = pPrintNoColor . WS.flipParseWhite   a
parse lang _ = tokenize lang

eval :: PrintLogs -> TypeOptions -> Compile -> AsciiLabels -> Lang -> Source -> IO ()
eval p options c a lang s = (controlTToIO p . evalParams lang) params
  where params = EvalParams {compile = c , asciiLabel = a , source = s , typeOptions = options}

evalParams :: BIO m => Lang -> EvalParams -> m ()
evalParams Cat = Cat.evalParams
evalParams Rev = Rev.evalParams
evalParams BF  = BF.evalParams
evalParams ETA = ETA.evalParams
evalParams SQ  = SQ.evalParams
evalParams STN = WS.evalParams VisibleTokenType
evalParams WS  = WS.evalParams WhiteTokenType
