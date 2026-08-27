{-# LANGUAGE FlexibleContexts #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet
  ( AdditionalColorStrategy (..)
  , CodelSize (..)
  , ImageConfig (..)
  , ImageReaderError (..)
  , MulticoloredCodelStrategy (..)
  , OptimizationLevel (..)
  , ParserError (..)
  , PietError (..)
  , PietStep (..)
  , graphText
  , nullReceiver
  ) where

import           Control.Monad.Except                                 ( MonadError, throwError )
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.CompileOption
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Parser
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.SyntaxVisualizer

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
          → m SyntaxGraph
makeGraph messageReceiver imageConfig inputPath = do
  messageReceiver StepReadImage
  codels <- mapError PietImageReaderError $ readCodels imageConfig inputPath
  messageReceiver StepParse
  mapError PietParserError $ parse codels

nullReceiver ∷ Monad m ⇒ PietStep → m ()
nullReceiver _ = pass

mapError ∷ MonadError e2 m ⇒ (e1 → e2) → ExceptT e1 m a → m a
mapError f = either (throwError . f) pure <=< runExceptT
