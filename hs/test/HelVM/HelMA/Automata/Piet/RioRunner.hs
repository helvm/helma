module HelVM.HelMA.Automata.Piet.RioRunner
  ( runTestEnv
  ) where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalOptions

import           HelVM.HelMA.Automata.Piet.API.Options ( simpleOptions )
import           HelVM.HelMA.LangCommand

import qualified Data.Text                             as Text
import qualified Data.Text.Lazy.Builder                as Builder
import qualified RIO

runTestEnv ∷ Text → RIO.RIO Env () → IO Text
runTestEnv inputText action = do
  outputRef ← newIORef mempty
  inputRef  ← newIORef inputText
  flip RIO.runRIO action $ buildTestEnv outputRef inputRef
  toText . Builder.toLazyText <$> readIORef outputRef

buildTestEnv ∷ IORef Builder.Builder → IORef Text → Env
buildTestEnv outputRef inputRef = Env
  { envFileIO     = testFileIO
  , envStdIO      = testStdIO outputRef inputRef
  , envOptions = testAppOptions
  , envLogFunc    = RIO.mkLogFunc (\_ _ _ _ → pass)
  }

testStdIO ∷ IORef Builder.Builder → IORef Text → StdIO
testStdIO outputRef inputRef = StdIO
  { stdPutLTextLn      = \t → modifyIORef outputRef (<> Builder.fromLazyText t <> Builder.singleton '\n')
  , stdGetContentsText = fromStrict <$> readIORef inputRef
  , stdPutLBSLn        = const pass
  , stdGetContentsBS   = pure mempty
  , stdPutChar         = \c → modifyIORef outputRef (<> Builder.singleton c)
  , stdGetChar         = getCharFrom inputRef
  , stdPutChars        = \t → modifyIORef outputRef (<> Builder.fromText t)
  , stdGetChars        = getCharsFrom inputRef
  }

getCharFrom ∷ IORef Text → IO Char
getCharFrom ref = go =<< readIORef ref where
  go t = maybe (fail "RioRunner: unexpected EOF") (\(c, rest) → writeIORef ref rest $> c) (Text.uncons t)

getCharsFrom ∷ IORef Text → IO Text
getCharsFrom ref = do
  t ← readIORef ref
  let (line, rest) = Text.break (== '\n') t
  writeIORef ref (Text.drop 1 rest)
  pure line

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
