{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelMA.Automaton.Eff.MonadEff (

  Element,
  AppEff,
  MonadEff,

  putAsChar,
  putAsDec,
  getCharAs,
  getDecAs,

--  putIntAsChar,
--  putIntAsDec,
--  getCharAsInt,
--  getDecAsInt,

  getContentsBS,
  getContentsText,
  getContents,
  getChar,
  putChar,
  eGetLine,
  ePutText,
  ePutTextLn,
  ePutLTextLn,
  eFlush,

  log,
  logError,
  logWarn,
  logInfo,
  logDebug,
) where

import           HelVM.HelIO.Control.Control
import           HelVM.HelMA.Automaton.API.LogLevel

import           HelVM.HelIO.ReadText

import qualified Data.ByteString.Lazy               as LByteString
import           Data.Default                       as Default
import qualified Data.Text.IO                       as Text
import qualified Data.Text.Lazy.IO                  as LText

import           Prelude                            hiding (getLine, putLTextLn, putText, putTextLn)
import qualified Prelude
import qualified RIO

import qualified System.IO                          as IO

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
type AppEff m = (MonadControl m , MonadEff m)

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
  getContents     :: m String
  getChar         :: m Char
  eGetLine         :: m Text
  putChar         :: Char -> m ()
  ePutText         :: Text -> m ()
  ePutTextLn       :: Text -> m ()
  ePutLTextLn      :: LText -> m ()
  eFlush           :: m ()

  log              :: LogLevel -> Text -> m ()

  putAsChar    = putIntAsChar . fromIntegral
  putAsDec     = putIntAsDec  . fromIntegral
  getCharAs    = fromIntegral <$> getCharAsInt
  getDecAs     = fromIntegral <$> getDecAsInt

  putIntAsChar = putChar . chr
  putIntAsDec  = ePutText . show
  getCharAsInt = ord <$> getChar
  getDecAsInt  = readTextUnsafe <$> eGetLine

  ePutTextLn s  = ePutText $ s <> "\n"
  ePutLTextLn   = ePutTextLn . toText
  eFlush        = pass

logError :: MonadEff m => Text -> m ()
logError = log Error

logWarn :: MonadEff m => Text -> m ()
logWarn = log Warn

logInfo :: MonadEff m => Text -> m ()
logInfo = log Info

logDebug :: MonadEff m => Text -> m ()
logDebug = log Debug

instance MonadEff IO where
  getContentsBS   = LByteString.getContents
  getContentsText = LText.getContents
  getContents     = IO.getContents
  getChar         = IO.getChar
  eGetLine         = Prelude.getLine
  putChar         = IO.putChar
  ePutText         = Prelude.putText
  ePutTextLn       = Prelude.putTextLn
  ePutLTextLn      = Prelude.putLTextLn
  eFlush           = flushIO
  log              = logIO

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) => MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  getContentsText = lift getContentsText
  getContents     = lift getContents
  getChar         = lift getChar
  eGetLine         = lift eGetLine
  putChar         = lift . putChar
  ePutText         = lift . ePutText
  ePutTextLn       = lift . ePutTextLn
  ePutLTextLn      = lift . ePutLTextLn
  eFlush           = lift eFlush
  log              = (lift .) . log

instance RIO.HasLogFunc env => MonadEff (RIO.RIO env) where
  getContentsBS   = liftIO LByteString.getContents
  getContentsText = liftIO LText.getContents
  getContents     = liftIO IO.getContents
  getChar         = liftIO IO.getChar
  eGetLine         = liftIO Prelude.getLine
  putChar         = liftIO . IO.putChar
  ePutText         = liftIO . Prelude.putText
  ePutTextLn       = liftIO . Prelude.putTextLn
  ePutLTextLn      = liftIO . Prelude.putLTextLn
  eFlush           = liftIO flushIO
  log l            = RIO.logGeneric "" (toRioLevel l) . RIO.display

---- Internal

flushIO :: IO ()
flushIO = hFlush stdout

logIO :: LogLevel -> Text -> IO ()
logIO l m = Text.hPutStrLn stderr $ logToTextLn (l , m)

toRioLevel :: LogLevel -> RIO.LogLevel
toRioLevel Error = RIO.LevelError
toRioLevel Warn  = RIO.LevelWarn
toRioLevel Info  = RIO.LevelInfo
toRioLevel Debug = RIO.LevelDebug
