module HelVM.HelMA.Automata.Piet.EvaluatorCollisionSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Piet.Evaluator
import           HelVM.HelMA.Automata.Piet.FileExtra

import           HelVM.HelMA.Automata.Piet.API.ImplType

import           HelVM.HelMA.Automaton.Eff.Mock

import           HelVM.GoldenExpectations

import           System.FilePath.Posix                  ( (<.>), (</>) )

import           Test.Hspec                             ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "Piet Interpreter Golden Tests" $ do
    let dirName  = "pietcc"
    let fileName = "hi"
    let input    = ""
    let cs       = Just 4

    let filePath = dirName </> fileName <.> "png"
    let fullPath = "examples" </> "piet" </> filePath
    let img = readImage fullPath

    let mock = (ioExecDynamicMockEffWithInput(toText input) . simpleEval Collision Nothing) =<< img
    let path = "free" </> dirName </> fileName <> input

    describe path $ do
      it ("output" </> path) $
        calculateDynamicOutput <$> mock `goldenShouldIO` buildAbsolutePietOutFileName path

      it ("logged" </> path) $
        calculateDynamicLogs <$> mock `goldenShouldIO` buildAbsolutePietLogFileName path
