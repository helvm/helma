module HelVM.HelMA.Automata.Piet.Hi.EvaluatorSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Piet.FileExtra
import           HelVM.HelMA.Automata.Piet.Hi.Main

import           HelVM.HelMA.Automaton.Eff.Mock

import           HelVM.HelIO.CartesianProduct

import           HelVM.GoldenExpectations

import qualified Codec.Picture                       as Picture

import           System.FilePath.Posix

import           Test.Hspec                          ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "Piet Hi Main Golden Tests" $ forM_ (
    -- [ ("99bottles"          , [""])
    -- , ("artsy_hello_world"  , [""])
    -- , ("fizzbuzz"           , [""])
    -- , ("pi_big"            , [""])
    -- , ("piet_hello_world"   , [""])
    -- , ("valentines"         , [""])
    -- ] >*< ["pint"]
    -- ) <> (
    -- [ ("99bottles"           , [""])
    -- , ("fizzbuzz"            , [""])
    -- , ("GameOfLife"          , [""])
    -- , ("hello_world_c1"      , [""])
    -- , ("hello_world_c4"      , [""])
    -- , ("hello_world_globe"   , [""])
    -- , ("helloworld-mondrian" , [""])
    -- , ("hw5"                 , [""])
    -- , ("japh"                , [""])
    -- , ("piet_pi"             , [""])
    -- , ("power2"              , ["0\n0\n"])
    -- , ("primetest2"          , ["0\n"])
    -- ] >*< ["rpiet"]
    -- ) <> (
    -- [ ("99bottles"           , [""])
    -- , ("adder"               , ["0\n0\n"])
    -- , ("alpha_filled"        , [""])
    -- , ("fizzbuzz"            , [""])
    -- , ("hi"                  , [""])
    [ ("hi"                  , [""])
    -- , ("hw"                  , [""])
    -- , ("hw1-1"               , [""])
    -- , ("hw5"                 , [""])
    -- , ("hw5_big"             , [""])
    -- , ("hw_large"            , [""])
    -- , ("ILoveYouLaura"       , [""])
    -- , ("piet_factorial"      , ["0\n0\n"])
    -- , ("piet_pi"             , [""])
    -- , ("piet_pi_big"         , [""])
    -- , ("power2"              , ["0\n0\n"])
    -- , ("primetest2"          , ["0\n"])
    -- , ("test2"               , [""])
    -- , ("test2_upscaled"      , [""])
    -- , ("test_break"          , [""])
    ] >*< ["pietcc"]
    ) $ \((fileName , inputs) , dirName ) -> do
    let filePath = dirName </> fileName <.> "png"
    let fullPath = "examples" </> "piet" </> filePath
    forM_ inputs $ \input -> do
      let mock = do
            dynamicImg <- readImage fullPath
            case dynamicImg of
              Picture.ImageRGB8 img -> ioExecMockEffWithInput (toText input) $ execute 1 img
              _                     -> error "Unsupported image format in test"
      let path = dirName </> fileName <> input
      describe path $ do
        it ("output" </> path) $
          calculateOutput <$> mock `goldenShouldIO` buildAbsolutePietOutFileName path
        it ("logged" </> path) $
          calculateLogsWithLevelDebug <$> mock `goldenShouldIO` buildAbsolutePietLogFileName path
