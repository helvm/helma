module HelVM.HelMA.Automata.WhiteSpace.Evaluator
  ( evalParams
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams  as S
import           HelVM.HelMA.Automata.WhiteSpace.Token

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions    as Automaton
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Automaton

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.LabelType

import           HelVM.HelIO.Control.Safe

import           Prelude                                       hiding (swap)

import qualified RIO

import           Text.Pretty.Simple

runRio ∷ Has env ⇒ TokenType → RIO.RIO env ()
runRio t = runWithOptions =<< optionsRio where
  runWithOptions o = run (App.emit o) t . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit → TokenType → EvalParams → RIO.RIO env ()
run No   t                = runAsRIO . evalParams t
run IL   VisibleTokenType = putLTextLnRio . pShowNoColor . (flipParseVisible <$> formatType <*> source)
run IL   WhiteTokenType   = putLTextLnRio . pShowNoColor . (flipParseWhite   <$> formatType <*> source)
run TL   VisibleTokenType = putLTextLnRio . show . tokenizeVisible . source
run TL   WhiteTokenType   = putLTextLnRio . show . tokenizeWhite   . source
run Code VisibleTokenType = putLTextLnRio . show . readVisibleTokens . source
run Code WhiteTokenType   = putLTextLnRio . show . readWhiteTokens   . source


simpleEval ∷ AppEff m ⇒ S.SimpleParams → m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.formatType p) $ S.automatonOptions p

----

evalParams ∷ AppEff m ⇒ TokenType → EvalParams → m ()
evalParams tokenType p = eval tokenType (source p) (formatType p) $ automatonOptions p

eval ∷ AppEff m ⇒ TokenType → Source → LabelType → Automaton.AutomatonOptions → m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL ∷ AppEff m ⇒ TokenList → LabelType → Automaton.AutomatonOptions → m ()
evalTL tl ascii ao = flip start ao =<< liftSafe (parseFromTL ascii tl)
