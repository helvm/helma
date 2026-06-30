module HelVM.HelMA where

import qualified HelVM.HelMA.Automaton.API.AppOptions            as App
import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.Lang
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Types.FileType
import           HelVM.HelMA.Automaton.Types.LabelType
import           HelVM.HelMA.Automaton.Types.TokenType

import qualified HelVM.HelMA.Automata.Cat.Evaluator              as Cat

import qualified HelVM.HelMA.Automata.Rev.Evaluator              as Rev

import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator        as BF
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser as BF

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser as BF_Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser as BF_Tree

import qualified HelVM.HelMA.Automata.ETA.Evaluator              as ETA
import qualified HelVM.HelMA.Automata.ETA.Lexer                  as ETA
import qualified HelVM.HelMA.Automata.ETA.Parser                 as ETA

import qualified HelVM.HelMA.Automata.FALSE.Parser               as F

import qualified HelVM.HelMA.Automata.LazyK.Evaluator            as Lazy

import qualified HelVM.HelMA.Automata.SubLeq.Evaluator           as SQ
import qualified HelVM.HelMA.Automata.SubLeq.Lexer               as SQ

import qualified HelVM.HelMA.Automata.WhiteSpace.Evaluator       as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Lexer           as WS
import qualified HelVM.HelMA.Automata.WhiteSpace.Parser          as WS

import qualified HelVM.HelMA.Automata.Zot.Automaton              as Zot

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           Text.Pretty.Simple

actualMain :: AppEff m => App.AppOptions -> m ()
actualMain = runNoBuffering =<< App.fileType

runNoBuffering :: FileType -> AppEff m => App.AppOptions -> m ()
runNoBuffering BinaryFile o = runBinary o
runNoBuffering TextFile   o = runText o

runBinary :: AppEff m => App.AppOptions -> m ()
-- runBinary o = Piet.actualMain $ Piet.PietOptions { program = Just $ App.file o, codelSize = App.codelSize o, verbosity = App.verbosity o }
runBinary = runText

runText :: AppEff m => App.AppOptions -> m ()
runText o = do
  source <- readSourceFile (App.exec o) (App.file o)
  run (App.emit o) (App.langWithOptions o) (App.evalParams o source)

readSourceFile :: AppEff m => Exec -> String -> m Source
readSourceFile True = pure . toText
readSourceFile _    = eReadFileText

run :: AppEff m => Emit -> LangWithOptions -> EvalParams -> m ()
run No   l r = evalParams                 l r
run IL   l r = ePutLTextLn $ parse        l (formatType r) (source r)
run TL   l r = ePutTextLn  $ tokenize     l (source r)
run Code l r = ePutTextLn  $ minification l (source r)

minification :: LangWithOptions -> Source -> Text
minification (LangWithOptions BF  _ _ _               ) = show . BF.readTokens
minification (LangWithOptions ETA _ _ _               ) = show . ETA.readTokens
minification (LangWithOptions SQ  _ _ _               ) = show . SQ.readSymbols
minification (LangWithOptions WS  _ _ VisibleTokenType) = show . WS.readVisibleTokens
minification (LangWithOptions WS  _ _ WhiteTokenType  ) = show . WS.readWhiteTokens
minification  _                                         = show

tokenize :: LangWithOptions -> Source -> Text
tokenize (LangWithOptions ETA _ _ _               ) = show . ETA.tokenize
tokenize (LangWithOptions WS  _ _ VisibleTokenType) = show . WS.tokenizeVisible
tokenize (LangWithOptions WS  _ _ WhiteTokenType  ) = show . WS.tokenizeWhite
tokenize (LangWithOptions SQ  _ _ _               ) = show . SQ.tokenize
tokenize  _                                         = show

parse :: LangWithOptions -> LabelType -> Source -> LText
parse (LangWithOptions BF   FastType _ _               ) _ = pShowNoColor . BF_Fast.parseAsListSafe
parse (LangWithOptions BF   TreeType _ _               ) _ = pShowNoColor . BF_Tree.parseAsVectorSafe
parse (LangWithOptions ETA  _        _ _               ) _ = pShowNoColor . ETA.parseSafe
parse (LangWithOptions F    _        _ _               ) _ = pShowNoColor . F.parseSafe
parse (LangWithOptions WS   _        _ VisibleTokenType) f = pShowNoColor . WS.flipParseVisible f
parse (LangWithOptions WS   _        _ WhiteTokenType  ) f = pShowNoColor . WS.flipParseWhite   f
parse  l                                                 _ = toLazy . tokenize l

evalParams :: AppEff m => LangWithOptions -> EvalParams -> m ()
evalParams (LangWithOptions BF   i _ _) = BF.evalParams i
evalParams (LangWithOptions Cat  _ _ _) = Cat.evalParams
evalParams (LangWithOptions ETA  _ i _) = ETA.evalParams i
evalParams (LangWithOptions F    _ _ _) = error "FALSE is not supported now"
evalParams (LangWithOptions Lazy _ _ _) = Lazy.evalParams
evalParams (LangWithOptions Rev  _ _ _) = Rev.evalParams
evalParams (LangWithOptions SQ   _ _ _) = SQ.evalParams
evalParams (LangWithOptions WS   _ _ t) = WS.evalParams t
evalParams (LangWithOptions Zot  _ _ _) = Zot.evalParams
evalParams (LangWithOptions Piet _ _ _) = error "Piet is not supported"
