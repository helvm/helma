module HelVM.HelMA.Automaton.Eff.MockEff (
  ioExecMockEffBatch,
  ioExecMockEffWithInput,

  safeExecMockEffBatch,
  safeExecMockEffWithInput,

  execMockEffBatch,
  execMockEffWithInput,

  runMockEff,
  createMockIOData,
  calculateOutput,

  calculateLogsWithLevelInfo,
  calculateLogsWithLevelDebug,
  calculateLogsWithLevel,

  MockEff,
  MockEffData (..),
  MockIOData (..),
  MockLogs,
  MockLog (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MockIO   (MockIOData (..), createMockIOData, mockDataPutChar, mockDataPutText, splitStringByLn)
import qualified HelVM.HelMA.Automaton.Eff.MockIO   as MockIO

import           HelVM.HelMA.Automaton.Eff.MockLog  (MockLog (..), MockLogs)
import qualified HelVM.HelMA.Automaton.Eff.MockLog  as MockLog

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Logger
import           Control.Monad.Trans.Writer.CPS     (Writer, runWriter)
import           Control.Monad.Writer.Class         (MonadWriter, tell)

import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as Text

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
runMockEff i mockIO = safeToMockData $ runWriter $ runStateT (unMockEff mockIO) $ createMockIOData i where
  safeToMockData ((Right _, io), logs) = MockEffData io logs
  safeToMockData ((Left msgs, io), logs) = MockEffData io (logs Seq.|> errLog) where
    errLog = MockLog defaultLoc "" LevelError $ toLogStr $ errorsToText msgs

calculateOutput :: MockEffData -> Output
calculateOutput = MockIO.calculateOutput . ioData

calculateLogsWithLevelInfo :: MockEffData -> Output
calculateLogsWithLevelInfo = MockLog.calculateLogsWithLevelInfo . logs

calculateLogsWithLevelDebug :: MockEffData -> Output
calculateLogsWithLevelDebug = MockLog.calculateLogsWithLevelDebug . logs

calculateLogsWithLevel :: LogLevel -> MockEffData -> Output
calculateLogsWithLevel t = MockLog.calculateLogsWithLevel t . logs

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
  mockGetContents' :: MonadMockEff m => MockIOData -> m String
  mockGetContents' mockIO = content <$ put mockIO { input = "" } where content = input mockIO

mockGetChar :: MonadMockEff m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockEff m => MockIOData -> m Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , Text.show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , Text.show mockIO) $ discard $ input mockIO }

mockGetLine :: MonadMockEff m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockEff m => MockIOData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockGetCharSafe :: MonadSafeMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadSafeMockEff m => MockIOData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , Text.show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadSafeMockEff m => m Text
mockGetLineSafe = mockGetLineSafe' =<< get where
  mockGetLineSafe' :: MonadSafeMockEff m => MockIOData -> m Text
  mockGetLineSafe' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: MonadMockEff m => Char -> m ()
mockPutChar = modify . mockDataPutChar

mockPutText :: MonadMockEff m => Text -> m ()
mockPutText = modify . mockDataPutText

mockLog :: MonadMockLogs m => MockLog -> m ()
mockLog = tell . one

----

type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m)

type MonadMockEff m = MonadState MockIOData m

type MonadMockLogs m = MonadWriter MockLogs m

newtype MockEff a = MockEff
  { unMockEff :: StateT MockIOData (Writer MockLogs) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState MockIOData
    , MonadWriter MockLogs
    )

data MockEffData = MockEffData
  { ioData :: !MockIOData
  , logs   :: !MockLogs
  }
  deriving stock (Eq , Show)
