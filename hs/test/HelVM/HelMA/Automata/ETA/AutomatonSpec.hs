module HelVM.HelMA.Automata.ETA.AutomatonSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.Common.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                            (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $ forM_ ((
    [ ("hello"   , [""])
    , ("hello2"  , [""])
    , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n"])
    , ("bottles" , [""])
    , ("crlf"    , [""])
    ] |><| ["original"]
    ) <> (
    [ ("true"    , [""])
    , ("hello"   , [""])
    , ("hello2"  , [""])
    , ("hello3"  , [""])
    , ("hello4"  , [""])
    , ("readnum" , ["0\n" , "1\n"])
    , ("fact"    , ["0\n" , "1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
    , ("bottles" , [""])
    ] |><| ["from-eas"]
    )) $ \((fileName , inputs) , dirName) -> do
      let filePath = dirName </> fileName
      let file = readEtaFile filePath
      forM_ (inputs |><| [False]) $ \ (input , compile) -> do
        let params = (compile ,  , defaultStackType) <$> file
        let mock = ioExecMockIOWithInput (toText input) . simpleEval =<< params
        let path = showCompile compile </> filePath <> input
        describe path $ do
          it ("output" </> path) $
            calculateOutput <$> mock `goldenShouldIO` buildAbsoluteEtaOutFileName path
          it ("logged" </> path) $
            calculateLogged <$> mock `goldenShouldIO` buildAbsoluteEtaLogFileName path
