module HelVM.HelMA.Automata.BrainFuck.EvaluatorSpec
    ( spec
    ) where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator
import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.CellType

import           HelVM.HelIO.CartesianProduct

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                  (Spec, describe, it)

spec ∷ Spec
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
    ] >>*< [Int8Type , Word8Type] <>
    [ ("helloWorld"            , ""     )
    , ("fascistHelloWorld"     , ""     )
--    , ("theShortestHelloWorld" , ""     )
    , ("99botles"              , ""     )
    , ("triangle"              , ""     )
    ] >>*< [Int16Type , Word16Type]
    ) >*< testedBfTypes) $ \((fileName , input , cellType) , implType) -> do
      let file = readBfFile fileName
      let params = (implType , , cellType) <$> file
      let exec = ioExecMockEffWithInput input . simpleEval =<< params
      let path = show implType </> show cellType </> fileName
      describe path $ do
        it ("output" </> path) $
          calculateOutput <$> exec `goldenShouldIO` buildAbsoluteBfOutFileName path
        it ("logged" </> path) $
          calculateLogsWithLevelInfo <$> exec `goldenShouldIO` buildAbsoluteBfLogFileName path

testedBfTypes ∷ [ImplType]
testedBfTypes = [defaultImplType]
--testedBfTypes = implTypes
