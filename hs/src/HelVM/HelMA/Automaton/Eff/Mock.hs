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

-- NEWTYPE & INSTANCES

newtype Mock a
  = Mock { unMock :: StateT MockEffData (Writer MockLoggerData) a }
  deriving newtype (Applicative, Functor, Monad, MonadState MockEffData, MonadWriter MockLoggerData)

type DynamicMockData = (MockEffData , (LogLevel , MockLoggerData))

type MockData = (MockEffData , MockLoggerData)

instance MonadEff Mock where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetChar
  getChars        = mockGetChars
  putChar         = mockPutChar
  putChars        = mockPutChars
  {-# INLINE getContentsBS #-}
  {-# INLINE getContentsText #-}
  {-# INLINE getChar #-}
  {-# INLINE getChars #-}
  {-# INLINE putChar #-}
  {-# INLINE putChars #-}

instance MonadEff (SafeT Mock) where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getChar         = mockGetCharSafe
  getChars        = mockGetCharsSafe
  putChar         = mockPutChar
  putChars        = mockPutChars
  {-# INLINE getContentsBS #-}
  {-# INLINE getContentsText #-}
  {-# INLINE getChar #-}
  {-# INLINE getChars #-}
  {-# INLINE putChar #-}
  {-# INLINE putChars #-}

instance {-# OVERLAPPING #-} MonadLogger Mock where
  monadLoggerLog loc src level msg = mockLog $ MockLog loc src level $ toLogStr msg
  {-# INLINE monadLoggerLog #-}

-- EXPORTED EXECUTION FUNCTIONS

ioExecDynamicMockEffWithInput ∷ Input → SafeT Mock () → IO DynamicMockData
ioExecDynamicMockEffWithInput i = safeToIO . safeExecDynamicMockEffWithInput i
{-# INLINABLE ioExecDynamicMockEffWithInput #-}

safeExecDynamicMockEffWithInput ∷ Input → SafeT Mock () → Safe DynamicMockData
safeExecDynamicMockEffWithInput i = pure . runDynamicMockEff i . runSafeT
{-# INLINABLE safeExecDynamicMockEffWithInput #-}

ioExecMockEffBatch ∷ SafeT Mock () → IO MockData
ioExecMockEffBatch = ioExecMockEffWithInput ""
{-# INLINABLE ioExecMockEffBatch #-}

ioExecMockEffWithInput ∷ Input → SafeT Mock () → IO MockData
ioExecMockEffWithInput i = safeToIO . safeExecMockEffWithInput i
{-# INLINABLE ioExecMockEffWithInput #-}

safeExecMockEffBatch ∷ SafeT Mock () → Safe MockData
safeExecMockEffBatch = safeExecMockEffWithInput ""
{-# INLINABLE safeExecMockEffBatch #-}

safeExecMockEffWithInput ∷ Input → SafeT Mock () → Safe MockData
safeExecMockEffWithInput i action = pure $ runMockEff i $ runSafeT action
{-# INLINABLE safeExecMockEffWithInput #-}

execMockEffBatch ∷ Mock () → MockData
execMockEffBatch = execMockEffWithInput ""
{-# INLINABLE execMockEffBatch #-}

execMockEffWithInput ∷ Input → Mock () → MockData
execMockEffWithInput i action = runMockEff i $ Right <$> action
{-# INLINABLE execMockEffWithInput #-}

-- RUNNERS

runDynamicMockEff ∷ Input → Mock (Safe ()) → DynamicMockData
runDynamicMockEff i mockEff = safeToMockData $ runWriter $ runStateT (unMock mockEff) $ createMockEffData i where
  safeToMockData ((Right _, io), logs)   = (io, (LevelInfo, logs))
  safeToMockData ((Left msgs, io), logs) = (io, (LevelDebug, addMsgs msgs logs) )
{-# INLINABLE runDynamicMockEff #-}

runMockEff ∷ Input → Mock (Safe ()) → MockData
runMockEff i mockEff = safeToMockData $ runWriter $ runStateT (unMock mockEff) $ createMockEffData i where
  safeToMockData ((Right _, io), logs)   = (io, logs)
  safeToMockData ((Left msgs, io), logs) = (io, addMsgs msgs logs)
{-# INLINABLE runMockEff #-}

-- LOG CALCULATORS & HELPERS

addMsgs ∷ Messages → Seq MockLog → Seq MockLog
addMsgs msgs logs = logs Seq.|> errLog msgs
{-# INLINE addMsgs #-}

errLog ∷ Messages → MockLog
errLog msgs = MockLog defaultLoc "" LevelError $ toLogStr $ errorsToText msgs
{-# INLINE errLog #-}

calculateDynamicOutput ∷ DynamicMockData → Output
calculateDynamicOutput = reverseOutput . fst
{-# INLINE calculateDynamicOutput #-}

calculateDynamicLogs ∷ DynamicMockData → Output
calculateDynamicLogs = uncurry filterLogsWithLevel . snd
{-# INLINE calculateDynamicLogs #-}

calculateOutput ∷ MockData → Output
calculateOutput = reverseOutput . fst
{-# INLINE calculateOutput #-}

calculateLogsWithLevelInfo ∷ MockData → Output
calculateLogsWithLevelInfo = filterLogsWithLevelInfo . snd
{-# INLINE calculateLogsWithLevelInfo #-}

calculateLogsWithLevelDebug ∷ MockData → Output
calculateLogsWithLevelDebug = filterLogsWithLevelDebug . snd
{-# INLINE calculateLogsWithLevelDebug #-}
