{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelMA.Automaton.Eff.MockEff (
  ioExecMockEffBatch,
  ioExecMockEffWithInput,

  safeExecMockEffBatch,
  safeExecMockEffWithInput,

  execMockEffBatch,
  execMockEffWithInput,

  runMockEff,
  createMockEff,
  calculateOutput,
  calculateLogged,

  MockEff,
  MockEffData,
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Message

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Logger

import qualified Data.ListLike                      as LL
import           Data.Text                          as Text

ioExecMockEffBatch :: SafeT MockEff () -> IO MockEffData
ioExecMockEffBatch = ioExecMockEffWithInput ""

ioExecMockEffWithInput :: Input -> SafeT MockEff () -> IO MockEffData
ioExecMockEffWithInput i = safeToIO . safeExecMockEffWithInput i

safeExecMockEffBatch :: SafeT MockEff () -> Safe MockEffData
safeExecMockEffBatch = safeExecMockEffWithInput ""

safeExecMockEffWithInput :: Input -> SafeT MockEff () -> Safe MockEffData
safeExecMockEffWithInput i action = pure $ runMockEff i $ runSafeT action

execMockEffBatch :: MockEff () -> MockEffData
execMockEffBatch = execMockEffWithInput ""

execMockEffWithInput :: Input -> MockEff () -> MockEffData
execMockEffWithInput i action = runMockEff i $ Right <$> action

----

runMockEff :: Input -> MockEff (Safe ()) -> MockEffData
runMockEff i mockIO = uncurry safeToMockData $ runState mockIO $ createMockEff i where
  safeToMockData (Left msgs) = mockDataLog $ MockLog defaultLoc  ""  LevelError  $ toLogStr $ errorsToText msgs
  safeToMockData (Right _  ) = id

createMockEff :: Input -> MockEffData
createMockEff i = MockEffData (toString i) "" []

calculateOutput :: MockEffData -> Output
calculateOutput = calculateText . output

calculateLogged :: MockEffData -> Output
calculateLogged d = Text.concat $ LL.reverse (decodeUtf8 . fromLogStr . logStr <$> logged d)

----

instance MonadEff MockEff where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetChar
  getLine         = mockGetLine
  putChar         = mockPutChar
  putTextEff      = mockPutText

instance MonadEff (SafeT MockEff) where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetCharSafe
  getLine         = mockGetLineSafe
  putChar         = mockPutChar
  putTextEff      = mockPutText

instance {-# OVERLAPPING #-} MonadLogger MockEff where
  monadLoggerLog loc src level msg = mockLog $ MockLog loc src level $ toLogStr msg

----

mockGetContentsBS :: MonadMockEff m => m LByteString
mockGetContentsBS = fromStrict . encodeUtf8 <$> mockGetContentsText

mockGetContentsText :: MonadMockEff m => m LText
mockGetContentsText = fromStrict . toText <$> mockGetContents

mockGetContents :: MonadMockEff m => m String
mockGetContents = mockGetContents' =<< get where
  mockGetContents' :: MonadMockEff m => MockEffData -> m String
  mockGetContents' mockIO = content <$ put mockIO { input = "" } where content = input mockIO

mockGetChar :: MonadMockEff m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockEff m => MockEffData -> m Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , Text.show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , Text.show mockIO) $ discard $ input mockIO }

mockGetLine :: MonadMockEff m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockEff m => MockEffData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockGetCharSafe :: MonadSafeMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadSafeMockEff m => MockEffData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , Text.show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadSafeMockEff m => m Text
mockGetLineSafe = mockGetLineSafe' =<< get where
  mockGetLineSafe' :: MonadSafeMockEff m => MockEffData -> m Text
  mockGetLineSafe' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: MonadMockEff m => Char -> m ()
mockPutChar = modify . mockDataPutChar

mockPutText :: MonadMockEff m => Text -> m ()
mockPutText = modify . mockDataPutText

mockLog :: MonadMockEff m => MockLog -> m ()
mockLog = modify . mockDataLog

----

mockDataPutChar :: Char -> MockEffData -> MockEffData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutText :: Text -> MockEffData -> MockEffData
mockDataPutText text mockIO = mockIO { output = calculateString text <> output mockIO }

mockDataLog :: MockLog -> MockEffData -> MockEffData
mockDataLog l mockIO = mockIO { logged = l : logged mockIO }

----

type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m)

type MonadMockEff m = MonadState MockEffData m

type MockEff = State MockEffData

calculateText :: String -> Output
calculateText = Text.reverse . toText

calculateString :: Output -> String
calculateString = toString . Text.reverse

data MockEffData = MockEffData
  { input  :: !String
  , output :: !String
  , logged :: !MockLogs
  }
  deriving stock (Eq , Show)

type MockLogs = [MockLog]

data MockLog = MockLog
  { logLoc    :: !Loc
  , logSource :: !LogSource
  , logLevel  :: !LogLevel
  , logStr    :: !LogStr
  }
  deriving stock (Eq, Show)

----

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
