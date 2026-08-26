{-# LANGUAGE FlexibleContexts #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet
  ( AdditionalColorStrategy (..)
  , CodelSize (..)
  , ImageConfig (..)
  , ImageReaderError (..)
  , MulticoloredCodelStrategy (..)
    -- , ObjectGeneratorError (..)
  , OptimizationLevel (..)
  , ParserError (..)
  , PietError (..)
  , PietStep (..)
    -- , compile
  , graphText
  , nullReceiver
  ) where

import           Control.Monad.Except                                 ( MonadError, throwError )
-- import           Data.Text.Lazy                                       ( LText )
-- import HelVM.HelMA.Automata.Piet.LLVM.Piet.AssemblyGenerator
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.CompileOption
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.ImageReader
-- import HelVM.HelMA.Automata.Piet.LLVM.Piet.JITRunner
-- import HelVM.HelMA.Automata.Piet.LLVM.Piet.ObjectGenerator
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Parser
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.SyntaxVisualizer
-- import qualified LLVM.AST                                             as AST

data PietError
  = PietImageReaderError ImageReaderError
  | PietParserError ParserError
  -- | PietObjectGeneratorError ObjectGeneratorError
  deriving stock (Eq, Show)

data PietStep
  = StepReadImage
  | StepParse
  | StepMakeAssembly
  | StepGenerateExecutable
  | StepRunJIT
  | StepGenerateDOT
  deriving stock (Eq, Show)

-- | Compile a Piet program.
-- compile ∷ ( MonadIO m
--            , MonadError PietError m
--            )
--         ⇒ (PietStep → m ())
--         → ImageConfig
--         → OptimizationLevel
--         → FilePath
--         → FilePath
--         → m ()
-- compile messageReceiver imageConfig optimizationLevel inputPath outputPath = do
--   ast <- makeAST messageReceiver imageConfig inputPath
--   messageReceiver StepGenerateExecutable
--   mapError PietObjectGeneratorError $ generateExecutable optimizationLevel outputPath ast

-- | Run a Piet program on JIT.
-- run ∷ ( MonadIO m
--        , MonadError PietError m
--        )
--     ⇒ (PietStep → m ())
--     → ImageConfig
--     → OptimizationLevel
--     → FilePath
--     → m ()
-- run messageReceiver imageConfig optimizationLevel inputPath = do
--   ast <- makeAST messageReceiver imageConfig inputPath
--   messageReceiver StepRunJIT
--   liftIO $ runJIT optimizationLevel ast

-- | Convert a Piet program to a graph script.
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

-- makeAST ∷ ( MonadIO m
--            , MonadError PietError m
--            )
--         ⇒ (PietStep → m ())
--         → ImageConfig
--         → FilePath
--         → m AST.Module
-- makeAST messageReceiver imageConfig inputPath = do
--   graph <- makeGraph messageReceiver imageConfig inputPath
--   messageReceiver StepMakeAssembly
--   pure $ generateAssembly graph

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
nullReceiver _ = pure ()

mapError ∷ MonadError e2 m ⇒ (e1 → e2) → ExceptT e1 m a → m a
mapError f = either (throwError . f) pure <=< runExceptT
