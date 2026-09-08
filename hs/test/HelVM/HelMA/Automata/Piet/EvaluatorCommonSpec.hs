module HelVM.HelMA.Automata.Piet.EvaluatorCommonSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Piet.Evaluator
import           HelVM.HelMA.Automata.Piet.FileExtra

import           HelVM.HelMA.Automaton.API.AutomatonType
import           HelVM.HelMA.Automaton.Eff.Mock

import           HelVM.HelIO.CartesianProduct

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec

spec ∷ Spec
spec =
  describe "Piet Interpreter Golden Tests" $ forM_ ((
    -- [ ("99bottles"          , [""])
    [ ("artsy_hello_world"  , [""])
    , ("fizzbuzz"           , [""])
    , ("pi_big"            , [""])
    , ("piet_hello_world"   , [""])
    , ("valentines"         , [""])
    ] >*< ["pint"]
    ) <> (
    -- [ ("99bottles"           , [""])
    -- , ("cowsay"              , [""])
    -- , ("euclid_clint"        , [""])
    [ ("fizzbuzz"            , [""])
    , ("GameOfLife"          , [""])
    , ("hello_world_c1"      , [""])
    , ("hello_world_c4"      , [""])
    , ("hello_world_globe"   , [""])
    , ("helloworld-mondrian" , [""])
    , ("hw5"                 , [""])
    , ("japh"                , [""])
    , ("piet_pi"             , [""])
    , ("power2"              , ["0\n0\n"])
    , ("primetest2"          , ["0\n"])
    ] >*< ["rpiet"]
    ) <> (
    -- [ ("99bottles"           , [""])
    [ ("adder"               , ["0\n0\n"])
    , ("alpha_filled"        , [""])
    -- , ("erat2"               , [""])
    -- , ("euclid_clint"        , [""])
    -- , ("euclid_clint_big"    , [""])
    , ("fizzbuzz"            , [""])
    , ("hi"                  , [""])
    , ("hw"                  , [""])
    , ("hw1-1"               , [""])
    , ("hw5"                 , [""])
    , ("hw5_big"             , [""])
    , ("hw_large"            , [""])
    , ("ILoveYouLaura"       , [""])
    -- , ("japh_big"            , [""])
    , ("piet_factorial"      , ["0\n0\n"])
    , ("piet_pi"             , [""])
    , ("piet_pi_big"         , [""])
    -- , ("pietquest"           , [""])
    , ("power2"              , ["0\n0\n"])
    -- , ("primetest"           , [""])
    , ("primetest2"          , ["0\n"])
    , ("test2"               , [""])
    , ("test2_upscaled"      , [""])
    -- , ("test3"               , [""])
    , ("test_break"          , [""])
    -- , ("test_push"           , [""])
    ] >*< ["pietcc"]
    )) $ \((fileName , inputs) , dirName ) -> do
    let filePath = dirName </> fileName <.> "png"
    let fullPath = "examples" </> "piet" </> filePath
    let img = readImage fullPath
    let implType = Common
    forM_ inputs $ \input -> do
      let mock = (ioExecMockEffWithInput (toText input) . simpleEval) =<< img
      let path = show implType </> dirName </> fileName <> input
      describe path $ do
        it ("output" </> path) $
          calculateOutput <$> mock `goldenShouldIO` buildAbsolutePietOutFileName path
        it ("logged" </> path) $
          calculateLogsWithLevelInfo <$> mock `goldenShouldIO` buildAbsolutePietLogFileName path
