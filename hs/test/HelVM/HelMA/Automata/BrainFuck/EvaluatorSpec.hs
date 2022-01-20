module HelVM.HelMA.Automata.BrainFuck.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator
import           HelVM.HelMA.Automata.BrainFuck.FileUtil

import           HelVM.Common.ZipA
import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.CellType

import           System.FilePath.Posix

import           Test.Hspec                               (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $ forM_ ((
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
--    , ("theShortestHelloWorld" , ""     )
    , ("99botles"              , ""     )
    , ("triangle"              , ""     )
    ] >><| [Int16Type , Word16Type]
    ) |><| options) $ \((fileName , input , cellType) , compile) -> do
      let file = readBfFile fileName
      let params = (compile , , cellType) <$> file
      let exec = ioExecMockIOWithInput input . simpleEval =<< params
      let path = compileToFilePath compile </> show cellType </> fileName
      describe path $ do
        it ("output" </> path) $
          calculateOutput <$> exec `goldenShouldIO` buildAbsoluteBfOutFileName path
        it ("logged" </> path) $
          calculateLogged <$> exec `goldenShouldIO` buildAbsoluteBfLogFileName path
