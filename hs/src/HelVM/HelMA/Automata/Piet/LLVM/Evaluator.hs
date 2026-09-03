module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( graphText
  , nullReceiver
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

graphText ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → FilePath → m LText
graphText messageReceiver imageConfig inputPath = messageReceiver StepGenerateDOT *> (syntaxToDOT <$> makeGraph messageReceiver imageConfig inputPath)

makeGraph ∷ MonadEvaluator m ⇒ (PietStep → m ()) → ImageConfig → FilePath → m (Maybe SyntaxGraph)
makeGraph messageReceiver imageConfig inputPath = messageReceiver StepParse *> (parse =<< messageReceiver StepReadImage *> (readCodels imageConfig =<< readImageFile inputPath))

readImageFile ∷ MonadEvaluator m ⇒ FilePath → m DynamicImage
readImageFile filePath = liftEitherLegacy =<< liftIO (readImage filePath)

nullReceiver ∷ Monad m ⇒ PietStep → m ()
nullReceiver _ = pass
