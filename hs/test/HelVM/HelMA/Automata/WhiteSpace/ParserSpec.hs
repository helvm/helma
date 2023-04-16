module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.FileExtra
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Control.Applicative.Tools

import           System.FilePath.Posix                       hiding ((<.>))

import           Test.Hspec                                  (Spec, describe, it)

spec :: Spec
spec =
  describe "parser" $ forM_ allFiles $ \ ((formatLabel , tokenType , dirName) , fileName) -> do
      let path = dirName </> fileName
      let outputPath = tokenTypeToExt tokenType </> path
      describe path $ do
        it ("minified" </> outputPath) $
          minifyFile tokenType path `goldenShouldIO` buildAbsoluteStnFileName outputPath
        it ("parsed"  </> outputPath) $
          optimizeFile NoOptimizations formatLabel tokenType path `goldenShouldIO` buildAbsoluteWsIlFileName ("parsed" </> outputPath)
        it ("optimized" </> outputPath) $
          optimizeFile AllOptimizations formatLabel tokenType path `goldenShouldIO` buildAbsoluteWsIlFileName ("optimized" </> outputPath)

allFiles :: [((FormatType , TokenType , FilePath) , FilePath)]
allFiles = originalW <> originalV <> fromWsa <> binaryLabel

originalW :: [((FormatType , TokenType , FilePath) , FilePath)]
originalW = [(TextLabel , WhiteTokenType , "original")] >*<
  [ "count"
  , "calc"
  , "fact"
  , "hanoi"
  , "name"
  ]

originalV :: [((FormatType , TokenType , FilePath) , FilePath)]
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

fromWsa :: [((FormatType, TokenType, FilePath) , FilePath)]
fromWsa = [(TextLabel , VisibleTokenType , "from-wsa")] >*<
  [ "true"
  , "hello"
  , "hello2"
  , "hello4"
  , "bottles"
  , "prim"
  ]

binaryLabel :: [((FormatType, TokenType, FilePath) , FilePath)]
binaryLabel = [(BinaryLabel , WhiteTokenType , "from-elvm")] >*<
  [ "hello"
  , "fizzbuzz"
  , "8cc.c.eir"
  ]

minifyFile :: TokenType -> String -> IO Text
minifyFile tokenType = readTokensByTokenType tokenType <.> readFileByTokenType tokenType

optimizeFile :: OptimizationLevel -> FormatType -> TokenType -> String -> IO Text
optimizeFile optLevel labelType tokenType path = safeIOToIO ((printIL <.> optimize optLevel <.> parseForTest labelType tokenType) <$> readFileByTokenType tokenType path)

readTokensByTokenType:: TokenType -> Text -> Text
readTokensByTokenType WhiteTokenType   = show . readWhiteTokens
readTokensByTokenType VisibleTokenType = show . readVisibleTokens
