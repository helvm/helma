{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module HelVM.HelMA.Automaton.Eff.Mock
  ( Mock (..)
  , MockEffData
  , MockLog (..)
  , MockLoggerData
  , calculateLogsWithLevelDebug
  , calculateLogsWithLevelInfo
  , calculateOutput
  , createMockEffData
  , execMockEffBatch
  , execMockEffWithInput
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

runMockEff ∷ Input → Mock (Safe ()) → MockData
runMockEff i mockEff = safeToMockData $ runWriter $ runStateT (unMock mockEff) $ createMockEffData i where
  safeToMockData ((Right _, io), logs) = (io, logs)
  safeToMockData ((Left msgs, io), logs) = (io, logs Seq.|> errLog) where
    errLog = MockLog defaultLoc "" LevelError $ toLogStr $ errorsToText msgs

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

type MockData = (MockEffData , MockLoggerData)
