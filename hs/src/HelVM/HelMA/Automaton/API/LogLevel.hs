module HelVM.HelMA.Automaton.API.LogLevel where

import qualified Control.Monad.Logger as Logger

import qualified RIO

import           Data.Default

showEitherTextLogLevel :: Either Text LogLevel -> Text
showEitherTextLogLevel (Right l) = show l
showEitherTextLogLevel (Left t)  = show t

fromLogger :: Logger.LogLevel -> Either Text LogLevel
fromLogger Logger.LevelDebug     = Right Debug
fromLogger Logger.LevelInfo      = Right Info
fromLogger Logger.LevelWarn      = Right Warn
fromLogger Logger.LevelError     = Right Error
fromLogger (Logger.LevelOther t) = Left t

fromRio :: RIO.LogLevel -> Either Text LogLevel
fromRio RIO.LevelDebug     = Right Debug
fromRio RIO.LevelInfo      = Right Info
fromRio RIO.LevelWarn      = Right Warn
fromRio RIO.LevelError     = Right Error
fromRio (RIO.LevelOther t) = Left t

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
    Debug
  | Info
  | Warn
  | Error
  deriving stock (Bounded , Enum , Eq , Ord, Read , Show)

instance Default LogLevel where
  def = minBound
