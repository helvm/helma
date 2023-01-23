module HelVM.HelMA.Automata.ETA.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Evaluator
import           HelVM.HelMA.Automata.ETA.FileExtra
import           HelVM.HelMA.Automata.ETA.SimpleParams

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                               (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $ forM_ ((
    [ ("hello"    , [""])
    , ("hello2"   , [""])
    , ("crlf"     , [""])
    ] |><| (["original"] |><< (etaImplTypes |><| [False]))
    ) <> (
    [ ("bottles"  , [""])
    , ("fact"     , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n"])
    ] |><| (["original"] |><< ([defaultETAImplType] |><| [False , True]))
    ) <> (
    [ ("true"     , [""])
    , ("hello"    , [""])
    , ("hello2"   , [""])
    , ("hello3"   , [""])
    , ("hello4"   , [""])
    , ("readnum"  , ["0\n" , "1\n"])
    , ("fact"     , ["0\n" , "1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
    , ("bottles"  , [""])
    , ("divmod"   , [""])
    , ("readchar" , ["A"])
    ] |><| (["from-eas"] |><< (etaImplTypes |><| [False]))
    )) $ \((fileName , inputs) , (dirName , implType, compile)) -> do
      let filePath = dirName </> fileName
      let file = readEtaFile filePath
      forM_ inputs $ \ input -> do
        let params = simpleParams implType defaultStackType compile <$> file
        let mock = ioExecMockIOWithInput (toText input) . simpleEval =<< params
        let path = show implType </> show compile </> filePath <> input
        describe path $ do
          it ("output" </> path) $
            calculateOutput <$> mock `goldenShouldIO` buildAbsoluteEtaOutFileName path
          it ("logged" </> path) $
            calculateLogged <$> mock `goldenShouldIO` buildAbsoluteEtaLogFileName path

