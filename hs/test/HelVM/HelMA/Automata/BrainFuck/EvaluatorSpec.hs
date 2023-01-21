module HelVM.HelMA.Automata.BrainFuck.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator
import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.CellType

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                (Spec, describe, it)

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
--    , ("99botles"              , ""     ) --FIXME to slow
    , ("triangle"              , ""     )
    ] >><| [Int16Type , Word16Type]
    ) |><| testedBfTypes) $ \((fileName , input , cellType) , bfType) -> do
      let file = readBfFile fileName
      let params = (bfType , , cellType) <$> file
      let exec = ioExecMockIOWithInput input . simpleEval =<< params
      let path = show bfType </> show cellType </> fileName
      describe path $ do
        it ("output" </> path) $
          calculateOutput <$> exec `goldenShouldIO` buildAbsoluteBfOutFileName path
        it ("logged" </> path) $
          calculateLogged <$> exec `goldenShouldIO` buildAbsoluteBfLogFileName path

testedBfTypes :: [BFType]
--testedBfTypes = [defaultBFType]
testedBfTypes = bfTypes
