{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelMA.Automaton.Eff.MonadEff (
  logErrorLegacy,
  logWarnLegacy,
  logInfoLegacy,
  logDebugLegacy,
  Element,
  AppEff,
  MonadEff(..),
) where

import           HelVM.HelIO.Control.Safe
import qualified HelVM.HelMA.Automaton.API.LogLevel as Legacy

import           HelVM.HelIO.ReadText

import           Control.Monad.Logger

import qualified Data.ByteString.Lazy               as LByteString
import           Data.Default                       as Default
import qualified Data.Text.IO                       as Text
import qualified Data.Text.Lazy.IO                  as LText

import           Prelude                            hiding (getLine, putText)
import qualified Prelude

import qualified RIO

import qualified System.IO                          as IO

logErrorLegacy :: MonadEff m => Text -> m ()
logErrorLegacy = logCurry Legacy.Error

logWarnLegacy :: MonadEff m => Text -> m ()
logWarnLegacy = logCurry Legacy.Warn

logInfoLegacy :: MonadEff m => Text -> m ()
logInfoLegacy = logCurry Legacy.Info

logDebugLegacy :: MonadEff m => Text -> m ()
logDebugLegacy = logCurry Legacy.Debug

logCurry :: MonadEff m => Legacy.LogLevel -> Text -> m ()
logCurry = curry log

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
type AppEff m = (MonadLogger m, MonadSafe m , MonadEff m)

class Monad m => MonadEff m where

  putAsChar       :: Integral v => v -> m ()
  putAsDec        :: Integral v => v -> m ()
  getCharAs       :: Integral v => m v
  getDecAs        :: Integral v => m v

  putIntAsChar    :: Int -> m ()
  putIntAsDec     :: Int -> m ()
  getCharAsInt    :: m Int
  getDecAsInt     :: m Int

  getContentsBS   :: m LByteString
  getContentsText :: m LText
  getChar         :: m Char
  getLine         :: m Text
  putChar         :: Char -> m ()
  putTextEff      :: Text -> m ()

  flush           :: m ()

  log             :: Legacy.Log -> m ()

  putAsChar      = putIntAsChar . fromIntegral
  putAsDec       = putIntAsDec  . fromIntegral
  getCharAs      = fromIntegral <$> getCharAsInt
  getDecAs       = fromIntegral <$> getDecAsInt

  putIntAsChar   = putChar . chr
  putIntAsDec    = putTextEff . show
  getCharAsInt   = ord <$> getChar
  getDecAsInt    = readTextUnsafe <$> getLine

  flush          = pass

instance MonadEff IO where
  getContentsBS   = LByteString.getContents
  getContentsText = LText.getContents
  getChar         = IO.getChar
  getLine         = Prelude.getLine
  putChar         = IO.putChar
  putTextEff      = Prelude.putText
  flush           = flushIO
  log             = logIO

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) => MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  getContentsText = lift getContentsText
  getChar         = lift getChar
  getLine         = lift getLine
  putChar         = lift . putChar
  putTextEff      = lift . putTextEff
  flush           = lift flush
  log             = lift . log

instance RIO.HasLogFunc env => MonadEff (RIO.RIO env) where
  getContentsBS   = liftIO LByteString.getContents
  getContentsText = liftIO LText.getContents
  getChar         = liftIO IO.getChar
  getLine         = liftIO Prelude.getLine
  putChar         = liftIO . IO.putChar
  putTextEff      = liftIO . Prelude.putText
  flush           = liftIO flushIO
  log             = logRIO

---- Internal

flushIO :: IO ()
flushIO = hFlush stdout

logIO :: Legacy.Log -> IO ()
logIO = Text.hPutStrLn stderr . Legacy.logToTextLn

logRIO :: RIO.HasLogFunc env => Legacy.Log -> RIO.RIO env ()
logRIO (l , m) = RIO.logGeneric "" (toRioLevel l) $ RIO.display m

toRioLevel :: Legacy.LogLevel -> RIO.LogLevel
toRioLevel Legacy.Error = RIO.LevelError
toRioLevel Legacy.Warn  = RIO.LevelWarn
toRioLevel Legacy.Info  = RIO.LevelInfo
toRioLevel Legacy.Debug = RIO.LevelDebug
