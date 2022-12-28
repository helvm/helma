module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.SubLeq.Evaluator
import           HelVM.HelMA.Automata.SubLeq.FileExtra

import           HelVM.HelMA.Automaton.Types.RAMType

import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO

import           System.FilePath.Posix

import           Test.Hspec                            (Spec, describe, it)

spec :: Spec
spec = describe "eval" $ forM_
  [ ("hello"               , "" )
  , ("longHello"           , "" )
  , ("esolangs/helloWorld" , "" )
  , ("mazonka/hi"          , "" )
  , ("mazonka/helloWorld"  , "" )
  , ("mazonka/factorial"   , "" )
--  , ("99-bottles-of-beer"  , "" )
--  , ("eForth"    , "" )
  ] $ \(fileName , input)  -> do
  let file = readSqFile fileName
  let mock = (ioExecMockIOWithInput input . simpleEval defaultRAMType) =<< file
  describe fileName $ do
    it ("output" </> fileName) $
      (calculateOutput <$> mock) `goldenShouldIO` buildAbsoluteSqOutFileName fileName
    it ("logged" </> fileName) $
      (calculateLogged <$> mock) `goldenShouldIO` buildAbsoluteSqLogFileName fileName
