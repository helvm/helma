module HelVM.HelMA.Automata.BrainFuck.Evaluator.TEvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator.TEvaluator
import           HelVM.HelMA.Automata.BrainFuck.FileUtil

import           HelVM.Common.ZipA
import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.CellType

import           System.FilePath.Posix

import           Test.Hspec                                          (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $
    forM_ (
      [ ("value256"              , ""     )
      , ("helloWorld"            , ""     )
      , ("fascistHelloWorld"     , ""     )
      , ("padHelloWorld"         , ""     )
      , ("theShortestHelloWorld" , ""     )
      , ("99botles"              , ""     )
      , ("triangle"              , ""     )
      , ("fibonacci"             , "0\r\n")
      ] >><| [Int8Type , Word8Type] <>
      [ ("helloWorld"            , ""     )
      , ("fascistHelloWorld"     , ""     )
--      , ("theShortestHelloWorld" , ""     )
      , ("99botles"              , ""     )
      , ("triangle"              , ""     )
      ] >><| [Int16Type , Word16Type]
      ) $ \(fileName , input , cellType) -> do
        let file = readBfFile fileName
        let params = ( , cellType) <$> file
        let exec = execMockIOWithInput input . uncurryEval <$> params
        let minorPath = show cellType </> fileName
        describe minorPath $ do
          it ("output" </> minorPath) $
            calculateOutput <$> exec `goldenShouldIO` buildAbsoluteOutFileName ("output" </> minorPath)
          it ("logged" </> minorPath) $
            calculateLogged <$> exec `goldenShouldIO` buildAbsoluteOutFileName ("logged" </> minorPath)
