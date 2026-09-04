module HelVM.HelMA.Automata.FALSE.Evaluator where

import           HelVM.HelMA.Automata.FALSE.Parser

import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import qualified RIO
import           Text.Pretty.Simple                   ( pShowNoColor )

runRio ∷ Has env ⇒ RIO.RIO env ()
runRio = runWIthOptions =<< optionsRio where
  runWIthOptions o = run (App.emit o) . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit → EvalParams → RIO.RIO env ()
run No = const $ error "FALSE is not supported now"
run IL = putLTextLnRio <=< runAsRIO . emitTL . source
run TL = putLTextLnRio <=< runAsRIO . emitTL . source
run _  = fallback

emitTL ∷ MonadSafe m ⇒ Source → m LText
emitTL s = liftSafe $ pShowNoColor <$> parseSafe s
