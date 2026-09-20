module HelVM.HelMA.Automata.Piet.RioRunner
  ( runTestEnv
  ) where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalOptions

import           HelVM.HelMA.Automata.Piet.API.Options ( simpleOptions )
import           HelVM.HelMA.LangCommand

import qualified Data.Text.Lazy.Builder                as Builder
import qualified RIO

runTestEnv ∷ Text → RIO.RIO Env () → IO Text
runTestEnv inputText action = do
  outputRef ← newIORef mempty
  inputRef  ← newIORef (toString inputText)
  let stdio   = testStdIO outputRef inputRef
  let fileIO  = testFileIO
  let logFunc = RIO.mkLogFunc (\_ _ _ _ → pass)
  let env     = Env fileIO stdio testAppOptions logFunc
  RIO.runRIO env action
  toText . Builder.toLazyText <$> readIORef outputRef

testStdIO ∷ IORef Builder.Builder → IORef String → StdIO
testStdIO outputRef inputRef = StdIO
  { stdPutLTextLn      = \t → modifyIORef outputRef (<> Builder.fromText (toText t) <> Builder.singleton '\n')
  , stdGetContentsText = fromStrict . toText <$> readIORef inputRef
  , stdPutLBSLn        = const pass
  , stdGetContentsBS   = pure mempty
  , stdPutChar         = \c → modifyIORef outputRef (<> Builder.singleton c)
  , stdGetChar         = getCharFrom inputRef
  , stdPutChars        = \t → modifyIORef outputRef (<> Builder.fromText t)
  , stdGetChars        = getCharsFrom inputRef
  }

getCharFrom ∷ IORef String → IO Char
getCharFrom ref = do
  s ← readIORef ref
  case s of
    []     → fail "RioRunner: unexpected EOF"
    (c:cs) → writeIORef ref cs $> c

getCharsFrom ∷ IORef String → IO Text
getCharsFrom ref = do
  s ← readIORef ref
  let (line, rest) = break (== '\n') s
  writeIORef ref (drop 1 rest)
  pure (toText line)

testFileIO ∷ FileIO
testFileIO = FileIO
  { readTextFile = \fp → fail ("testFileIO: unexpected readTextFile: " <> fp)
  , readImage    = \fp → fail ("testFileIO: unexpected readImage: "    <> fp)
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
