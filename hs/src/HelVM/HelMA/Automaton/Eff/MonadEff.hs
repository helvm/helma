module HelVM.HelMA.Automaton.Eff.MonadEff (

  Element,
  AppEff,
  MonadEff,

  ePutAsChar,
  ePutAsDec,
  eGetCharAs,
  eGetDecAs,

--  ePutIntAsChar,
--  ePutIntAsDec,
--  eGetCharAsInt,
--  eGetDecAsInt,

  eGetContentsBS,
  eGetContentsText,
  eGetContents,
  eGetChar,
  ePutChar,
  eGetLine,
  ePutText,
  ePutTextLn,
  ePutLTextLn,
  eFlush,
  eLogText,
  eLogTextLn,
  eLogShow,

  logText,
  flush,
  eReadFileText,
) where

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra
import           HelVM.HelIO.ReadText

import qualified Data.ByteString.Lazy        as LByteString
import           Data.Default                as Default
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy.IO           as LText

import           System.IO                   hiding (getLine, hFlush, stderr, stdout)

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
type AppEff m = (MonadControl m , MonadEff m)

class Monad m => MonadEff m where

  ePutAsChar       :: Integral v => v -> m ()
  ePutAsDec        :: Integral v => v -> m ()
  eGetCharAs       :: Integral v => m v
  eGetDecAs        :: Integral v => m v

  ePutIntAsChar    :: Int -> m ()
  ePutIntAsDec     :: Int -> m ()
  eGetCharAsInt    :: m Int
  eGetDecAsInt     :: m Int

  eGetContentsBS   :: m LByteString
  eGetContentsText :: m LText
  eGetContents     :: m String
  eGetChar         :: m Char
  eGetLine         :: m Text
  ePutChar         :: Char -> m ()
  ePutText         :: Text -> m ()
  ePutTextLn       :: Text -> m ()
  ePutLTextLn      :: LText -> m ()
  eLogText         :: Text -> m ()
  eLogTextLn       :: Text -> m ()
  eLogShow         :: Show s => s -> m ()
  eFlush           :: m ()
  eReadFileText    :: FilePath -> m Text

  ePutAsChar    = ePutIntAsChar . fromIntegral
  ePutAsDec     = ePutIntAsDec  . fromIntegral
  eGetCharAs    = fromIntegral <$> eGetCharAsInt
  eGetDecAs     = fromIntegral <$> eGetDecAsInt

  ePutIntAsChar = ePutChar . chr
  ePutIntAsDec  = ePutText . show
  eGetCharAsInt = ord <$> eGetChar
  eGetDecAsInt  = readTextUnsafe <$> eGetLine

  ePutTextLn s  = ePutText $ s <> "\n"
  ePutLTextLn   = ePutTextLn . toText
  eLogTextLn s  = eLogText $ s <> "\n"
  eLogShow      = eLogTextLn . show
  eFlush        = pass

logText :: Text -> IO ()
logText = Text.hPutStrLn stderr

flush :: IO ()
flush = hFlush stdout

instance MonadEff IO where
  eGetContentsBS   = LByteString.getContents
  eGetContentsText = LText.getContents
  eGetContents     = getContents
  eGetChar         = getChar
  eGetLine         = getLine
  ePutChar         = putChar
  ePutText         = putText
  ePutTextLn       = putTextLn
  ePutLTextLn      = putLTextLn
  eLogText         = logText
  eFlush           = flush
  eReadFileText    = readFileTextUtf8

type ExceptTLegacy = ExceptT String

exceptTLegacy :: Monad m => m a -> ExceptTLegacy m a
exceptTLegacy a = ExceptT $ pure <$> a

instance MonadEff (ExceptT String IO) where --FIXXME
  eGetContentsBS   = exceptTLegacy   LByteString.getContents
  eGetContentsText = exceptTLegacy   LText.getContents
  eGetContents     = exceptTLegacy   getContents
  eGetChar         = exceptTLegacy   getChar
  eGetLine         = exceptTLegacy   getLine
  ePutChar         = exceptTLegacy . putChar
  ePutText         = exceptTLegacy . putText
  ePutTextLn       = exceptTLegacy . putTextLn
  ePutLTextLn      = exceptTLegacy . putLTextLn
  eLogText         = exceptTLegacy . logText
  eFlush           = exceptTLegacy   flush
  eReadFileText    = exceptTLegacy . readFileTextUtf8

instance MonadEff (SafeT IO) where
  eGetContentsBS   = safeT   LByteString.getContents
  eGetContentsText = safeT   LText.getContents
  eGetContents     = safeT   getContents
  eGetChar         = safeT   getChar
  eGetLine         = safeT   getLine
  ePutChar         = safeT . putChar
  ePutText         = safeT . putText
  ePutTextLn       = safeT . putTextLn
  ePutLTextLn      = safeT . putLTextLn
  eLogText         = safeT . logText
  eFlush           = safeT   flush
  eReadFileText    = safeT . readFileTextUtf8

instance MonadEff (ControlT IO) where
  eGetContentsBS   = controlT   LByteString.getContents
  eGetContentsText = controlT   LText.getContents
  eGetContents     = controlT   getContents
  eGetChar         = controlT   getChar
  eGetLine         = controlT   getLine
  ePutChar         = controlT . putChar
  ePutText         = controlT . putText
  ePutTextLn       = controlT . putTextLn
  ePutLTextLn      = controlT . putLTextLn
  eLogText         = controlT . logText
  eFlush           = controlT   flush
  eReadFileText    = controlT . readFileTextUtf8
