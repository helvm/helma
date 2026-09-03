module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( graphText
  , graphTextRio
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified HelVM.HelMA.Automaton.API.AppOptions        as App
import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelMA.Automaton.Extra

import qualified RIO

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

graphTextRio ∷ Has env ⇒ ImageConfig → RIO.RIO env LText
graphTextRio imageConfig = graphTextWithOptions =<< optionsRio where
  graphTextWithOptions o = graphText imageConfig (App.file o)

graphText ∷ Has env ⇒ ImageConfig → FilePath → RIO.RIO env LText
graphText imageConfig inputPath = do
  logStep StepGenerateDOT
  syntaxToDOT <$> makeGraph imageConfig inputPath

makeGraph ∷ Has env ⇒ ImageConfig → FilePath → RIO.RIO env (Maybe SyntaxGraph)
makeGraph imageConfig inputPath = do
  logStep StepParse
  logStep StepReadImage
  image <- readImageRio inputPath
  codels <- runAsRIO $ readCodels imageConfig image
  runAsRIO $ parse codels

logStep ∷ Has env ⇒ PietStep → RIO.RIO env ()
logStep step = RIO.logDebug (RIO.displayShow step)
