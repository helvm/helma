module HelVM.HelMA.Automata.WhiteSpace.ParserSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType
import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.FileExtra
import           HelVM.HelMA.Automata.WhiteSpace.Parser

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Types.LabelType

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.CartesianProduct

import           HelVM.GoldenExpectations

import           Control.Applicative.Tools

import           System.FilePath.Posix                         hiding ( (<.>) )

import           Test.Hspec                                    ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "parser" $ forM_ allFiles $ \ ((formatLabel , tokenType , dirName) , fileName) -> do
      let path = dirName </> fileName
      let outputPath = tokenTypeToExt tokenType </> path
      describe path $ do
        it ("minified" </> outputPath) $
          minifyFile tokenType path `goldenLShouldIO` buildAbsoluteStnFileName outputPath
        it ("parsed"  </> outputPath) $
          optimizeFile NoOptimizations formatLabel tokenType path `goldenLShouldIO` buildAbsoluteWsIlFileName ("parsed" </> outputPath)
        it ("optimized" </> outputPath) $
          optimizeFile AllOptimizations formatLabel tokenType path `goldenLShouldIO` buildAbsoluteWsIlFileName ("optimized" </> outputPath)

allFiles ∷ [((LabelType , TokenType , FilePath) , FilePath)]
allFiles = originalW <> originalV <> fromWsa <> binaryLabel

originalW ∷ [((LabelType , TokenType , FilePath) , FilePath)]
originalW = [(TextLabel , WhiteTokenType , "original")] >*<
  [ "count"
  , "calc"
  , "fact"
  , "hanoi"
  , "name"
  ]

originalV ∷ [((LabelType , TokenType , FilePath) , FilePath)]
originalV = [(TextLabel , VisibleTokenType , "original")] >*<
  [ "count"
  , "helloWorld"
  , "hWorld"
  , "calc"
  , "fact"
  , "hanoi"
  , "locTest"
  , "name"
  , "truthMachine"
  ]

fromWsa ∷ [((LabelType, TokenType, FilePath) , FilePath)]
fromWsa = [(TextLabel , VisibleTokenType , "from-wsa")] >*<
  [ "true"
  , "hello"
  , "hello2"
  , "hello4"
  , "bottles"
  , "prim"
  ]

binaryLabel ∷ [((LabelType, TokenType, FilePath) , FilePath)]
binaryLabel = [(BinaryLabel , WhiteTokenType , "from-elvm")] >*<
  [ "hello"
  , "fizzbuzz"
  , "8cc.c.eir"
  ]

minifyFile ∷ TokenType → String → IO LText
minifyFile tokenType = emitCode tokenType <.> readFileByTokenType tokenType

optimizeFile ∷ OptimizationLevel → LabelType → TokenType → String → IO LText
optimizeFile optLevel labelType tokenType path = safeIOToIO (emitILForTest optLevel labelType tokenType <$> readFileByTokenType tokenType path)

emitILForTest ∷ OptimizationLevel → LabelType → TokenType → Source → Safe LText
emitILForTest optLevel labelType tokenType = printIL <.> optimize optLevel <.> parseIL labelType tokenType
