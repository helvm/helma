module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( graphText
  , graphTextRio
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelIO.Control.Safe                    ( MonadSafe )
import qualified HelVM.HelMA.Automaton.API.AppOptions        as App
import           HelVM.HelMA.Automaton.API.Env

import qualified RIO

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

graphTextRio ∷ (Has env, MonadSafe (RIO.RIO env)) ⇒ (PietStep → RIO.RIO env ()) → ImageConfig → RIO.RIO env LText
graphTextRio messageReceiver imageConfig = graphTextWithOptions =<< optionsRio where
  graphTextWithOptions o = graphText messageReceiver imageConfig (App.file o)

graphText ∷ (Has env, MonadSafe (RIO.RIO env)) ⇒ (PietStep → RIO.RIO env ()) → ImageConfig → FilePath → RIO.RIO env LText
graphText messageReceiver imageConfig inputPath = do
  messageReceiver StepGenerateDOT
  syntaxToDOT <$> makeGraph messageReceiver imageConfig inputPath

makeGraph ∷ (Has env, MonadSafe (RIO.RIO env)) ⇒ (PietStep → RIO.RIO env ()) → ImageConfig → FilePath → RIO.RIO env (Maybe SyntaxGraph)
makeGraph messageReceiver imageConfig inputPath = do
  messageReceiver StepParse
  image <- messageReceiver StepReadImage *> readImageRio inputPath
  codels <- readCodels imageConfig image
  parse codels
