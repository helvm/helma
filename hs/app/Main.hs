module Main where

import qualified AppOptions                                      as App
import           BoolTypes
import           Emit
import           Lang

import qualified HelVM.HelMA.Automata.Cat.Automaton              as Cat

import qualified HelVM.HelMA.Automata.Rev.Automaton              as Rev

import qualified HelVM.HelMA.Automata.BrainFuck.Automaton        as BF
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser as BF

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser as BF_Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser as BF_Tree

import qualified HelVM.HelMA.Automata.ETA.Automaton              as ETA
import qualified HelVM.HelMA.Automata.ETA.Lexer                  as ETA
import qualified HelVM.HelMA.Automata.ETA.Parser                 as ETA

import qualified HelVM.HelMA.Automata.FALSE.Parser               as F

import qualified HelVM.HelMA.Automata.LazyK.Automaton            as Lazy

import qualified HelVM.HelMA.Automata.SubLeq.Automaton           as SQ
import qualified HelVM.HelMA.Automata.SubLeq.Lexer               as SQ

import qualified HelVM.HelMA.Automata.WhiteSpace.Automaton       as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Lexer           as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Parser          as WS

import qualified HelVM.HelMA.Automata.Zot.Automaton              as Zot

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams
import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           HelVM.HelIO.Control.Control

import           HelVM.HelIO.Extra

import           Options.Applicative
import           Text.Pretty.Simple

import qualified System.IO                                       as IO

main :: IO ()
main = runApp =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelMA: The Interpreter of BrainFuck , ETA , Lazy , SubLeq , WhiteSpace, Zot"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

runApp:: App.AppOptions -> IO ()
runApp (App.AppOptions emit printLogs lang bfType etaImplType tokenType compile formatType ramType stackType cellType intCellType dumpType exec file) = do
  hSetBuffering stdout IO.NoBuffering
  source <- readSourceFile exec file
  run emit printLogs langWithOptions (runParams source) where
    langWithOptions  = LangWithOptions lang bfType etaImplType tokenType
    runParams source = RunParams compile formatType source typeOptions
    typeOptions      = TypeOptions ramType stackType cellType intCellType dumpType

readSourceFile :: Exec -> String -> IO Source
readSourceFile True = pure . toText
readSourceFile _    = readFileTextUtf8

run :: Emit -> PrintLogs -> LangWithOptions -> RunParams -> IO ()
run No   p l r = (controlTToIO p . runWithParams l) r
run IL   _ l r = parse          l (formatType r) (source r)
run TL   _ l r = tokenize       l (source r)
run Code _ l r = minification   l (source r)

runWithParams :: BIO m => LangWithOptions-> RunParams -> m ()
runWithParams (LangWithOptions BF   i _ _) = BF.runWithParams i
runWithParams (LangWithOptions Cat  _ _ _) = Cat.runWithParams
runWithParams (LangWithOptions ETA  _ i _) = ETA.runWithParams i
runWithParams (LangWithOptions F    _ _ _) = error "FALSE is not supported now"
runWithParams (LangWithOptions Lazy _ _ _) = Lazy.runWithParams
runWithParams (LangWithOptions Rev  _ _ _) = Rev.runWithParams
runWithParams (LangWithOptions SQ   _ _ _) = SQ.runWithParams
runWithParams (LangWithOptions WS   _ _ t) = WS.runWithParams t
runWithParams (LangWithOptions Zot  _ _ _) = Zot.runWithParams

parse :: LangWithOptions -> FormatType -> Source -> IO ()
parse (LangWithOptions WS   _        _ VisibleTokenType) f = pPrintNoColor . WS.flipParseVisible f
parse (LangWithOptions WS   _        _ WhiteTokenType  ) f = pPrintNoColor . WS.flipParseWhite   f
parse (LangWithOptions BF   FastType _ _               ) _ = pPrintNoColor . BF_Fast.parseAsListSafe
parse (LangWithOptions BF   TreeType _ _               ) _ = pPrintNoColor . BF_Tree.parseAsVectorSafe
parse (LangWithOptions ETA  _        _ _               ) _ = pPrintNoColor . ETA.parseSafe
parse (LangWithOptions F    _        _ _               ) _ = pPrintNoColor . F.parseSafe
parse  l                                                 _ = tokenize l

tokenize :: LangWithOptions -> Source -> IO ()
tokenize (LangWithOptions WS  _ _ VisibleTokenType) = print . WS.tokenizeVisible
tokenize (LangWithOptions WS  _ _ WhiteTokenType  ) = print . WS.tokenizeWhite
tokenize (LangWithOptions ETA _ _ _               ) = print . ETA.tokenize
tokenize (LangWithOptions SQ  _ _ _               ) = print . SQ.tokenize
tokenize  _                                         = print

minification :: LangWithOptions -> Source -> IO ()
minification (LangWithOptions WS  _ _ VisibleTokenType) = print . WS.readVisibleTokens
minification (LangWithOptions WS  _ _ WhiteTokenType  ) = print . WS.readWhiteTokens
minification (LangWithOptions BF  _ _ _               ) = print . BF.readTokens
minification (LangWithOptions ETA _ _ _               ) = print . ETA.readTokens
minification (LangWithOptions SQ  _ _ _               ) = print . SQ.readSymbols
minification  _                                         = print
