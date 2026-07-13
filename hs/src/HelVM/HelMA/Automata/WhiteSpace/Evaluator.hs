module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  runWithOptions,
  run,
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S
import           HelVM.HelMA.Automata.WhiteSpace.Token

import qualified HelVM.HelMA.Automaton.API.AppOptions         as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions   as Automaton
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Automaton

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.LabelType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

import           Prelude                                      hiding (swap)

import qualified RIO

import           Text.Pretty.Simple

runWithOptions :: Has env => App.AppOptions -> RIO.RIO env ()
runWithOptions o = runAsRIO . run (App.emit o) (App.tokenType o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit -> TokenType -> EvalParams -> m ()
run No   t                = evalParams t
run IL   VisibleTokenType = ePutLTextLn . pShowNoColor . (flipParseVisible <$> formatType <*> source)
run IL   WhiteTokenType   = ePutLTextLn . pShowNoColor . (flipParseWhite   <$> formatType <*> source)
run TL   VisibleTokenType = ePutTextLn . show . tokenizeVisible . source
run TL   WhiteTokenType   = ePutTextLn . show . tokenizeWhite   . source
run Code VisibleTokenType = ePutTextLn . show . readVisibleTokens . source
run Code WhiteTokenType   = ePutTextLn . show . readWhiteTokens   . source


simpleEval :: AppEff m => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.formatType p) $ S.automatonOptions p

----

evalParams :: AppEff m => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (formatType p) $ automatonOptions p

eval :: AppEff m => TokenType -> Source -> LabelType -> Automaton.AutomatonOptions -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: AppEff m => TokenList -> LabelType -> Automaton.AutomatonOptions -> m ()
evalTL tl ascii ao = flip start ao =<< liftSafe (parseFromTL ascii tl)
