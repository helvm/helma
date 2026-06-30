{-# LANGUAGE UndecidableInstances #-}
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
  eReadFileText,

  eLogText,
  eLogTextLn,
  eLogShow,
) where

import           HelVM.HelIO.Control.Control

import           HelVM.HelIO.Extra
import           HelVM.HelIO.ReadText

import qualified Data.ByteString.Lazy        as LByteString
import           Data.Default                as Default
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy.IO           as LText

import qualified RIO

import qualified System.IO                   as IO

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
  eFlush           :: m ()
  eReadFileText    :: FilePath -> m Text

  eLogText         :: Text -> m ()
  eLogTextLn       :: Text -> m ()
  eLogShow         :: Show s => s -> m ()

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
  eFlush        = pass

  eLogTextLn s  = eLogText $ s <> "\n"
  eLogShow      = eLogTextLn . show

logText :: Text -> IO ()
logText = Text.hPutStrLn stderr

flush :: IO ()
flush = hFlush stdout

instance MonadEff IO where
  eGetContentsBS   = LByteString.getContents
  eGetContentsText = LText.getContents
  eGetContents     = IO.getContents
  eGetChar         = IO.getChar
  eGetLine         = getLine
  ePutChar         = IO.putChar
  ePutText         = putText
  ePutTextLn       = putTextLn
  ePutLTextLn      = putLTextLn
  eFlush           = flush
  eReadFileText    = readFileTextUtf8
  eLogText         = logText


instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) => MonadEff (t m) where
  eGetContentsBS   = lift eGetContentsBS
  eGetContentsText = lift eGetContentsText
  eGetContents     = lift eGetContents
  eGetChar         = lift eGetChar
  eGetLine         = lift eGetLine
  ePutChar         = lift . ePutChar
  ePutText         = lift . ePutText
  ePutTextLn       = lift . ePutTextLn
  ePutLTextLn      = lift . ePutLTextLn
  eFlush           = lift eFlush
  eReadFileText    = lift . eReadFileText
  eLogText         = lift . eLogText

instance RIO.HasLogFunc env => MonadEff (RIO.RIO env) where
  eGetContentsBS   = liftIO LByteString.getContents
  eGetContentsText = liftIO LText.getContents
  eGetContents     = liftIO IO.getContents
  eGetChar         = liftIO IO.getChar
  eGetLine         = liftIO getLine
  ePutChar         = liftIO . IO.putChar
  ePutText         = liftIO . putText
  ePutTextLn       = liftIO . putTextLn
  ePutLTextLn      = liftIO . putLTextLn
  eFlush           = liftIO flush
  eReadFileText    = liftIO . readFileTextUtf8
  eLogText         = RIO.logInfo . RIO.display
