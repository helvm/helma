module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( evaluate
  , graphText
  ) where

import           HelVM.HelMA.Automata.Piet.ImageReader
import           HelVM.HelMA.Automata.Piet.SyntaxParser

import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

-- Constraint Type Aliases

type MonadControl m = (MonadSafe m, MonadLogger m)

evaluate ∷ Has env ⇒ ImageConfig → FilePath → RIO.RIO env LText
evaluate imageConfig = (runAsRIO . graphText imageConfig) <=< readImageRio

graphText ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m LText
graphText imageConfig image = logStep StepGenerateDOT *> (syntaxToDOT <$> makeGraph imageConfig image)

makeGraph ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m (Maybe SyntaxGraph)
makeGraph imageConfig image = logStep StepParse *> (parse =<< readCodelsWithStep imageConfig image)

readCodelsWithStep ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
readCodelsWithStep imageConfig image = logStep StepReadImage *> readColors imageConfig image

logStep ∷ MonadLogger m ⇒ PietStep → m ()
logStep step = logDebugN $ show step
