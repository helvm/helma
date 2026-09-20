module HelVM.HelMA.Automata.Piet.EvaluatorCommonRIOSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Piet.Evaluator
import           HelVM.HelMA.Automata.Piet.FileExtra

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import qualified HelVM.HelMA.Automaton.API.Env         as Env
import           HelVM.HelMA.Automaton.API.EvalOptions

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automata.Piet.API.Options ( simpleOptions )
import           HelVM.HelMA.LangCommand

import           HelVM.HelIO.CartesianProduct

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import qualified RIO

import           Test.Hspec

spec ∷ Spec
spec =
  describe "Piet Interpreter Golden Tests (RIO)" $ forM_ ((
    [ ("99bottles"          , [""])
    , ("artsy_hello_world"  , [""])
    , ("fizzbuzz"           , [""])
    , ("pi_big"            , [""])
    , ("piet_hello_world"   , [""])
    , ("valentines"         , [""])
    ] >*< ["pint"]
    ) <> (
    [ ("99bottles"           , [""])
    -- , ("cowsay"              , [""])
    -- , ("euclid_clint"        , [""])
    , ("fizzbuzz"            , [""])
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
    [ ("99bottles"           , [""])
    , ("adder"               , ["0\n0\n"])
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
    )) $ \((fileName , inputs) , dirName) → do
    let filePath = dirName </> fileName <.> "png"
    let fullPath = "examples" </> "piet" </> filePath
    forM_ inputs $ \input → do
      let path   = "CommonRIO" </> dirName </> fileName <> input
      let img = readImageIO fullPath
        it path $ do
          let result = (runTestEnv (toText input) . void . runAsRIOResult . simpleEval) =<< img
          result `goldenShouldIO` buildAbsolutePietOutFileName path

runTestEnv ∷ Text → RIO.RIO Env.Env () → IO Text
runTestEnv inputText action = do
  outputRef ← newIORef (mempty ∷ String)
  inputRef  ← newIORef (toString inputText)
  let stdio   = testStdIO outputRef inputRef
  let fileIO  = testFileIO
  let logFunc = RIO.mkLogFunc (\_ _ _ _ → pass)
  let env     = Env.Env fileIO stdio testAppOptions logFunc
  RIO.runRIO env action
  toText <$> readIORef outputRef

testStdIO ∷ IORef String → IORef String → Env.StdIO
testStdIO outputRef inputRef = Env.StdIO
  { Env.stdPutLTextLn      = \t → modifyIORef outputRef (<> toString t <> "\n")
  , Env.stdGetContentsText = fromStrict . toText <$> readIORef inputRef
  , Env.stdPutLBSLn        = const pass
  , Env.stdGetContentsBS   = pure mempty
  , Env.stdPutChar         = \c → modifyIORef outputRef (<> [c])
  , Env.stdGetChar         = getCharFrom inputRef
  , Env.stdPutChars        = \t → modifyIORef outputRef (<> toString t)
  , Env.stdGetChars        = getCharsFrom inputRef
  }

getCharFrom ∷ IORef String → IO Char
getCharFrom ref = do
  s ← readIORef ref
  case s of
    []     → fail "EvaluatorCommonRIOSpec: unexpected EOF"
    (c:cs) → writeIORef ref cs $> c

getCharsFrom ∷ IORef String → IO Text
getCharsFrom ref = do
  s ← readIORef ref
  let (line, rest) = break (== '\n') s
  writeIORef ref (drop 1 rest)
  pure (toText line)

testFileIO ∷ Env.FileIO
testFileIO = Env.FileIO
  { Env.readTextFile = \fp → fail ("testFileIO: unexpected readTextFile: " <> fp)
  , Env.readImage    = \fp → fail ("testFileIO: unexpected readImage: "    <> fp)
  }

testAppOptions ∷ AppOptions
testAppOptions = AppOptions
  { verbosity   = minBound
  , emit        = No
  , exec        = False
  , evalOptions = simpleEvalOptions
  , langCommand = PietCommand simpleOptions
  , file        = ""
  }
