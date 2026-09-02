{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Evaluator
  ( graphText
  , nullReceiver
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           Control.Monad.Except                        ( MonadError, throwError )

data PietError
  = PietImageReaderError ImageReaderError
  | PietParserError ParserError
  deriving stock (Eq, Show)

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

graphText ∷ ( MonadIO m
             , MonadError PietError m
             )
          ⇒ (PietStep → m ())
          → ImageConfig
          → FilePath
          → m LText
graphText messageReceiver imageConfig inputPath = do
  graph <- makeGraph messageReceiver imageConfig inputPath
  messageReceiver StepGenerateDOT
  pure $ syntaxToDOT graph

makeGraph ∷ ( MonadIO m
             , MonadError PietError m
             )
          ⇒ (PietStep → m ())
          → ImageConfig
          → FilePath
          → m (Maybe SyntaxGraph)
makeGraph messageReceiver imageConfig inputPath = do
  messageReceiver StepReadImage
  codels <- mapError PietImageReaderError $ readCodels imageConfig inputPath
  messageReceiver StepParse
  mapError PietParserError $ parse codels

nullReceiver ∷ Monad m ⇒ PietStep → m ()
nullReceiver _ = pass

mapError ∷ MonadError e2 m ⇒ (e1 → e2) → ExceptT e1 m a → m a
mapError f = either (throwError . f) pure <=< runExceptT
