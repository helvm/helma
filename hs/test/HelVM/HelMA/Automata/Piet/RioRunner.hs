module HelVM.HelMA.Automata.Piet.RioRunner
  ( runTestEnv
  ) where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import qualified HelVM.HelMA.Automaton.API.Env         as Env
import           HelVM.HelMA.Automaton.API.EvalOptions

import           HelVM.HelMA.Automata.Piet.API.Options ( simpleOptions )
import           HelVM.HelMA.LangCommand

import qualified RIO

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
