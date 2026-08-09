{-# LANGUAGE GeneralisedNewtypeDeriving #-}

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

  MockEff (..),
  MockEffData,
  MockIOData (..),
  MockLogs,
  MockLog (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MockIO
import           HelVM.HelMA.Automaton.Eff.MockLogger
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import           Control.Monad.Logger
import           Control.Monad.Trans.Writer.CPS       (Writer, runWriter)
import           Control.Monad.Writer.Class           (MonadWriter)

import qualified Data.Sequence                        as Seq

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
  safeToMockData ((Right _, io), logs) = (io, logs)
  safeToMockData ((Left msgs, io), logs) = (io, logs Seq.|> errLog) where
    errLog = MockLog defaultLoc "" LevelError $ toLogStr $ errorsToText msgs

calculateOutput :: MockEffData -> Output
calculateOutput = reverseOutput . fst

calculateLogsWithLevelInfo :: MockEffData -> Output
calculateLogsWithLevelInfo = filterLogsWithLevelInfo . snd

calculateLogsWithLevelDebug :: MockEffData -> Output
calculateLogsWithLevelDebug = filterLogsWithLevelDebug . snd

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

type MockEffData = (MockIOData , MockLogs)
