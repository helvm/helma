module HelVM.HelMA.Automata.ETA.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.ETA.FileUtil

import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.StackType

import           System.FilePath.Posix

import           Test.Hspec                                     (Spec, describe, it)

spec :: Spec
spec = do
  describe "from-eas" $
    forM_
      [ ("true"    , [""])
      , ("hello"   , [""])
      , ("hello2"  , [""])
      , ("hello3"  , [""])
      , ("hello4"  , [""])
      , ("readnum" , ["0\n" , "1\n"])
      , ("fact"    , ["0\n" , "1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
      , ("bottles" , [""])
      ] $ \(fileName , inputs) -> do
        let file = readEtaFile ("from-eas" </> fileName)
        forM_ inputs $ \ input  -> do
          let params = (, defaultStackType) <$> file
          let mock = ioExecMockIOWithInput (toText input) . uncurryEval =<< params
          let minorPath = fileName <> input
          describe minorPath$ do
            it ("output" </> minorPath) $
              calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("from-eas" </> "output" </> minorPath)
            it ("logged" </> minorPath) $
              calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("from-eas" </> "logged" </> minorPath)

  describe "original" $
    forM_
      [ ("hello"   , [""])
      , ("hello2"  , [""])
      , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n"])
      , ("bottles" , [""])
      , ("crlf"    , [""])
      ] $ \(fileName , inputs) -> do
        let file = readEtaFile ("original" </> fileName)
        forM_ inputs $ \ input -> do
          let params = (, defaultStackType) <$> file
          let mock = ioExecMockIOWithInput (toText input) . uncurryEval =<< params
          let minorPath = fileName <> input
          describe minorPath $ do
            it ("output" </> minorPath) $
              calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("original" </> "output" </> minorPath)
            it ("logged" </> minorPath) $
              calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("original" </> "logged" </> minorPath)
