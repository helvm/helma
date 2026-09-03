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

evaluate ∷ MonadEvaluator m ⇒ ImageConfig → FilePath → m LText
evaluate imageConfig inputPath = graphText imageConfig =<< readImageFile inputPath

graphText ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m LText
graphText imageConfig image = logStep StepGenerateDOT *> (syntaxToDOT <$> makeGraph imageConfig image)

makeGraph ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m (Maybe SyntaxGraph)
makeGraph imageConfig image = logStep StepParse *> (parse =<< readCodelsWithStep imageConfig image)

readCodelsWithStep ∷ MonadControl m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
readCodelsWithStep imageConfig image = logStep StepReadImage *> readCodels imageConfig image

readImageFile ∷ MonadEvaluator m ⇒ FilePath → m DynamicImage
readImageFile filePath = liftEitherLegacy =<< liftIO (readImage filePath)

logStep ∷ MonadLogger m ⇒ PietStep → m ()
logStep step = logDebugN $ show step
