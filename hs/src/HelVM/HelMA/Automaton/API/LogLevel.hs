module HelVM.HelMA.Automaton.API.LogLevel where

import qualified Control.Monad.Logger as Logger

import qualified RIO

import           Data.Default

toLogger :: LogLevel -> Logger.LogLevel
toLogger Debug = Logger.LevelDebug
toLogger Info  = Logger.LevelInfo
toLogger Warn  = Logger.LevelWarn
toLogger Error = Logger.LevelError

toRio :: LogLevel -> RIO.LogLevel
toRio Debug = RIO.LevelDebug
toRio Info  = RIO.LevelInfo
toRio Warn  = RIO.LevelWarn
toRio Error = RIO.LevelError

defaultLogLevel :: LogLevel
defaultLogLevel = def

logLevels :: NonEmpty LogLevel
logLevels = universeNonEmpty

data LogLevel =
    Error
  | Warn
  | Info
  | Debug
  deriving stock (Bounded , Enum , Eq , Ord, Read , Show)

instance Default LogLevel where
  def = minBound
