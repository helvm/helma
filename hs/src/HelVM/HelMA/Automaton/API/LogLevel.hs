module HelVM.HelMA.Automaton.API.LogLevel where

import qualified Control.Monad.Logger               as Logger

import qualified RIO

import           Data.Default

import           HelVM.HelIO.Containers.MTIndexSafe

logLevelFromInt ∷ Int → LogLevel
logLevelFromInt = fromInt logLevels

showEitherTextLogLevel ∷ Either Text LogLevel → Text
showEitherTextLogLevel (Right l) = show l
showEitherTextLogLevel (Left t)  = show t

fromLogger ∷ Logger.LogLevel → Either Text LogLevel
fromLogger Logger.LevelError     = Right Error
fromLogger Logger.LevelWarn      = Right Warn
fromLogger Logger.LevelInfo      = Right Info
fromLogger Logger.LevelDebug     = Right Debug
fromLogger (Logger.LevelOther t) = Left t

fromRio ∷ RIO.LogLevel → Either Text LogLevel
fromRio RIO.LevelError     = Right Error
fromRio RIO.LevelWarn      = Right Warn
fromRio RIO.LevelInfo      = Right Info
fromRio RIO.LevelDebug     = Right Debug
fromRio (RIO.LevelOther t) = Left t

toLogger ∷ LogLevel → Logger.LogLevel
toLogger Error = Logger.LevelError
toLogger Warn  = Logger.LevelWarn
toLogger Info  = Logger.LevelInfo
toLogger Debug = Logger.LevelDebug

toRio ∷ LogLevel → RIO.LogLevel
toRio Error = RIO.LevelError
toRio Warn  = RIO.LevelWarn
toRio Info  = RIO.LevelInfo
toRio Debug = RIO.LevelDebug

defaultLogLevel ∷ LogLevel
defaultLogLevel = def

logLevels ∷ NonEmpty LogLevel
logLevels = universeNonEmpty

data LogLevel
  = Error
  | Warn
  | Info
  | Debug
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

instance Default LogLevel where
  def = minBound

----

fromInt ∷ (Bounded a, Foldable t) ⇒ t a → Int → a
fromInt l i = fromMaybe maxBound $ indexMaybe (toList l) i
