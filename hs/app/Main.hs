module Main where

import           AppOptions
import           Lang

import qualified HelVM.HelMA.Automata.Cat.Automaton        as Cat

import qualified HelVM.HelMA.Automata.Rev.Automaton        as Rev

import qualified HelVM.HelMA.Automata.BrainFuck.Automaton  as BF
import qualified HelVM.HelMA.Automata.BrainFuck.Lexer      as BF

import qualified HelVM.HelMA.Automata.ETA.Automaton        as ETA
import qualified HelVM.HelMA.Automata.ETA.Lexer            as ETA

import qualified HelVM.HelMA.Automata.LazyK.API.ParserType as LK
import qualified HelVM.HelMA.Automata.LazyK.Automaton      as LK


import qualified HelVM.HelMA.Automata.SubLeq.Automaton     as SQ
import qualified HelVM.HelMA.Automata.SubLeq.Lexer         as SQ

import qualified HelVM.HelMA.Automata.WhiteSpace.Automaton as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Lexer     as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Parser    as WS

import qualified HelVM.HelMA.Automata.Zot.Automaton        as Zot


import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams
import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.SwitchEnum

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
runApp (AppOptions lang visibleTokens parserType minified emitTL emitIL printLogs compile asciiLabels ramType stackType cellType intCellType dumpType exec file) = do
  hSetBuffering stdout IO.NoBuffering
  source <- readSourceFile exec file
  run minified emitTL emitIL printLogs typeOptions asciiLabels compile (parseLang lang) (enumFromBool visibleTokens) (LK.parseParserType parserType) source
    where typeOptions = TypeOptions (parseRAMType ramType) (parseStackType stackType) (parseCellType cellType) (parseIntCellType intCellType) (parseDumpType dumpType)

readSourceFile :: Exec -> String -> IO Source
readSourceFile True = pure . toText
readSourceFile _    = readFileText

run :: Minified -> EmitTL -> EmitIL -> PrintLogs -> TypeOptions -> Compile -> AsciiLabels -> Lang -> TokenType -> LK.ParserType -> Source -> IO ()
run True _    _    _ _ _ _ l t _ s = minification l t s
run _    True _    _ _ _ _ l t _ s = tokenize l t s
run _    _    True _ _ _ a l t _ s = parse l t a s
run _    _    _    p o c a l t k s = eval p o c a l t k s

minification :: Lang -> TokenType -> Source -> IO ()
minification WS VisibleTokenType = print . WS.readVisibleTokens
minification WS WhiteTokenType   = print . WS.readWhiteTokens
minification BF  _               = print . BF.readTokens
minification ETA _               = print . ETA.readTokens
minification SQ  _               = print . SQ.readSymbols
minification _   _               = print

tokenize :: Lang -> TokenType -> Source -> IO ()
tokenize WS  VisibleTokenType = print . WS.tokenizeVisible
tokenize WS  WhiteTokenType   = print . WS.tokenizeWhite
tokenize ETA _                = print . ETA.tokenize
tokenize SQ  _                = print . SQ.tokenize
tokenize _   _                = print

parse :: Lang -> TokenType -> AsciiLabels -> Source -> IO ()
parse WS   VisibleTokenType a = pPrintNoColor . WS.flipParseVisible a
parse WS   WhiteTokenType   a = pPrintNoColor . WS.flipParseWhite   a
parse lang tt               _ = tokenize lang tt

eval :: PrintLogs -> TypeOptions -> Compile -> AsciiLabels -> Lang -> TokenType -> LK.ParserType -> Source -> IO ()
eval p options c a lang tt pt s = (controlTToIO p . runWithParams lang tt pt) params
  where params = RunParams {compile = c , asciiLabel = a , source = s , typeOptions = options}

runWithParams :: BIO m => Lang -> TokenType -> LK.ParserType -> RunParams -> m ()
runWithParams LK  _ p = LK.runWithParams p
runWithParams WS  t _ = WS.runWithParams t
runWithParams Cat _ _ = Cat.runWithParams
runWithParams Rev _ _ = Rev.runWithParams
runWithParams BF  _ _ = BF.runWithParams
runWithParams ETA _ _ = ETA.runWithParams
runWithParams SQ  _ _ = SQ.runWithParams
runWithParams Zot _ _ = Zot.runWithParams
