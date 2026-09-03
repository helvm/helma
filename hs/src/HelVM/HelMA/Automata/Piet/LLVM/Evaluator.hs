module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( evaluate
  , graphText
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           Codec.Picture

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

-- Constraint Type Aliases
type MonadEvaluator m = (MonadIO m, MonadSafe m)


evaluate ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → FilePath → m LText
evaluate messageReceiver imageConfig inputPath = graphText messageReceiver imageConfig =<< readImageFile inputPath

graphText ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → DynamicImage → m LText
graphText messageReceiver imageConfig image = messageReceiver StepGenerateDOT *> (syntaxToDOT <$> makeGraph messageReceiver imageConfig image)

makeGraph ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → DynamicImage → m (Maybe SyntaxGraph)
makeGraph messageReceiver imageConfig image = messageReceiver StepParse *> (parse =<< messageReceiver StepReadImage *> readCodels imageConfig image)

readImageFile ∷ MonadEvaluator m ⇒ FilePath → m DynamicImage
readImageFile filePath = liftEitherLegacy =<< liftIO (readImage filePath)
