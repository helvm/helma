module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.SubLeq.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.SubLeq.FileUtil

import           HelVM.HelMA.Automaton.Types.RAMType

import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO

import           System.FilePath.Posix

import           Test.Hspec                                        (Spec, describe, it)

spec :: Spec
spec =
  forM_ [ ("hello"               , "" )
        , ("longHello"           , "" )
        , ("esolangs/helloWorld" , "" )
        , ("mazonka/hi"          , "" )
        , ("mazonka/helloWorld"  , "" )
        , ("mazonka/factorial"   , "" )
--        , ("99-bottles-of-beer"  , "" )
--        , ("eForth"    , "" )
        ] $ \(fileName , input)  -> do
    let file = readSqFile fileName
    let mock = (ioExecMockIOWithInput input . flippedEval defaultRAMType) =<< file
    describe fileName $ do
      it "output" $
         (calculateOutput <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("output" </> fileName)
      it "logged" $
         (calculateLogged <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("logged" </> fileName)
