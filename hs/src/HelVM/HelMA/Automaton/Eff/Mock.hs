{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module HelVM.HelMA.Automaton.Eff.Mock
  ( Mock (..)
  , MockEffData
  , MockLog (..)
  , MockLoggerData
  , calculateDynamicLogs
  , calculateDynamicOutput
  , calculateLogsWithLevelDebug
  , calculateLogsWithLevelInfo
  , calculateOutput
  , createMockEffData
  , execMockEffBatch
  , execMockEffWithInput
  , ioExecDynamicMockEffWithInput
  , ioExecMockEffBatch
  , ioExecMockEffWithInput
  , runMockEff
  , safeExecMockEffBatch
  , safeExecMockEffWithInput
  ) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MockEff
import           HelVM.HelMA.Automaton.Eff.MockLogger
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import           Control.Monad.Logger
import           Control.Monad.Trans.Writer.CPS       ( Writer, runWriter )
import           Control.Monad.Writer.Class           ( MonadWriter )

import qualified Data.Sequence                        as Seq

ioExecDynamicMockEffWithInput ∷ Input → SafeT Mock () → IO DynamicMockData
ioExecDynamicMockEffWithInput i = safeToIO . safeExecDynamicMockEffWithInput i

safeExecDynamicMockEffWithInput ∷ Input → SafeT Mock () → Safe DynamicMockData
safeExecDynamicMockEffWithInput i = pure . runDynamicMockEff i . runSafeT

ioExecMockEffBatch ∷ SafeT Mock () → IO MockData
ioExecMockEffBatch = ioExecMockEffWithInput ""

ioExecMockEffWithInput ∷ Input → SafeT Mock () → IO MockData
ioExecMockEffWithInput i = safeToIO . safeExecMockEffWithInput i

safeExecMockEffBatch ∷ SafeT Mock () → Safe MockData
safeExecMockEffBatch = safeExecMockEffWithInput ""

safeExecMockEffWithInput ∷ Input → SafeT Mock () → Safe MockData
safeExecMockEffWithInput i action = pure $ runMockEff i $ runSafeT action

execMockEffBatch ∷ Mock () → MockData
execMockEffBatch = execMockEffWithInput ""

execMockEffWithInput ∷ Input → Mock () → MockData
execMockEffWithInput i action = runMockEff i $ Right <$> action

----

runDynamicMockEff ∷ Input → Mock (Safe ()) → DynamicMockData
runDynamicMockEff i mockEff = safeToMockData $ runWriter $ runStateT (unMock mockEff) $ createMockEffData i where
  safeToMockData ((Right _, io), logs)   = (io, (LevelInfo, logs))
  safeToMockData ((Left msgs, io), logs) = (io, (LevelDebug, addMsgs msgs logs) )

runMockEff ∷ Input → Mock (Safe ()) → MockData
runMockEff i mockEff = safeToMockData $ runWriter $ runStateT (unMock mockEff) $ createMockEffData i where
  safeToMockData ((Right _, io), logs)   = (io, logs)
  safeToMockData ((Left msgs, io), logs) = (io, addMsgs msgs logs)

addMsgs ∷ Messages → Seq MockLog → Seq MockLog
addMsgs msgs logs = logs Seq.|> errLog msgs

errLog ∷ Messages → MockLog
errLog msgs = MockLog defaultLoc "" LevelError $ toLogStr $ errorsToText msgs

calculateDynamicOutput ∷ DynamicMockData → Output
calculateDynamicOutput = reverseOutput . fst

calculateDynamicLogs ∷ DynamicMockData → Output
calculateDynamicLogs = uncurry filterLogsWithLevel . snd

calculateOutput ∷ MockData → Output
calculateOutput = reverseOutput . fst

calculateLogsWithLevelInfo ∷ MockData → Output
calculateLogsWithLevelInfo = filterLogsWithLevelInfo . snd

calculateLogsWithLevelDebug ∷ MockData → Output
calculateLogsWithLevelDebug = filterLogsWithLevelDebug . snd

----

instance MonadEff Mock where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetChar
  getLine         = mockGetLine
  putChar         = mockPutChar
  putLine         = mockPutLine

instance MonadEff (SafeT Mock) where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetCharSafe
  getLine         = mockGetLineSafe
  putChar         = mockPutChar
  putLine         = mockPutLine

instance {-# OVERLAPPING #-} MonadLogger Mock where
  monadLoggerLog loc src level msg = mockLog $ MockLog loc src level $ toLogStr msg

----

newtype Mock a
  = Mock { unMock :: StateT MockEffData (Writer MockLoggerData) a }
  deriving newtype (Applicative, Functor, Monad, MonadState MockEffData, MonadWriter MockLoggerData)

type DynamicMockData = (MockEffData , (LogLevel , MockLoggerData))

type MockData = (MockEffData , MockLoggerData)
