module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.FileUtil
import           HelVM.HelMA.Automata.WhiteSpace.SimpleParams

import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.Common.ZipA
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                   (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $ forM_ (((
    [ ("count"        , ""           )
    , ("hworld"       , ""           )
    , ("calc"         , "-1\n"       )
    , ("fact"         , "10\n"       )
    , ("hanoi"        , "1\n"        )
    , ("loctest"      , "1\n2\n"     )
    , ("name"         , "WriteOnly\n")
    ] |><| ["original"]) |><| [WhiteTokenType]
    ) <> ((
    [ ("count"        , ""           )
    , ("helloWorld"   , ""           )
    , ("hWorld"       , ""           )
    , ("calc"         , "-1\n"       )
    , ("fact"         , "10\n"       )
    , ("hanoi"        , "1\n"        )
    , ("locTest"      , "1\n2\n"     )
    , ("name"         , "WriteOnly\n")
    , ("truthMachine" , "0\n"        )
    ] |><| ["original"]) |><| [VisibleTokenType]
    ) <> (((
    [ "true"
    , "hello"
    , "hello2"
    , "hello4"
    , "bottles"
    , "prim"
    ] |><| [""]) |><| ["from-wsa"]) |><| [VisibleTokenType]
    )) $ \ (((fileName , input) , dirName) , tokenType) -> do
      let ext = tokenTypeToExt tokenType
      let filePath = dirName </> fileName
      let file = readExtFile ext filePath
      forM_ options $ \ ascii -> do
        let paramsF = simpleParamsWithDefaults tokenType ascii
        let paramsIO = paramsF <$> file
        let path = ext </> showAscii ascii </> filePath <> toString input
        let mock = ioExecMockIOWithInput input . simpleEval =<< paramsIO
        describe path $ do
          it ("output" </> path) $
            calculateOutput <$> mock `goldenShouldIO` buildAbsoluteWsOutFileName path
          it ("logged" </> path) $
            calculateLogged <$> mock `goldenShouldIO` buildAbsoluteWsLogFileName path
