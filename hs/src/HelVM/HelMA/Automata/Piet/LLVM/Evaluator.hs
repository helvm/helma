module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( evaluate
  , graphText
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Parser

import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

-- Constraint Type Aliases
type MonadEvaluator m = (MonadIO m, MonadControl m)

type MonadControl m = (MonadSafe m, MonadLogger m)

evaluate ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → FilePath → m LText
evaluate messageReceiver imageConfig inputPath = graphText messageReceiver imageConfig =<< readImageFile inputPath

readImageFile ∷ MonadEvaluator m ⇒ FilePath → m DynamicImage
readImageFile filePath = liftEitherLegacy =<< liftIO (readImage filePath)

graphText ∷ MonadControl m ⇒ (PietStep → m ()) → ImageConfig → DynamicImage → m LText
graphText messageReceiver imageConfig image = messageReceiver StepGenerateDOT *> (syntaxToDOT <$> makeGraph messageReceiver imageConfig image)

makeGraph ∷ MonadControl m ⇒ (PietStep → m ()) → ImageConfig → DynamicImage → m (Maybe SyntaxGraph)
makeGraph messageReceiver imageConfig image = messageReceiver StepParse *> (parse =<< readCodelsWithStep messageReceiver imageConfig image)

readCodelsWithStep ∷ MonadControl m ⇒ (PietStep → m ()) → ImageConfig → DynamicImage → m (Matrix Color)
readCodelsWithStep messageReceiver imageConfig image = messageReceiver StepReadImage *> readCodels imageConfig image
