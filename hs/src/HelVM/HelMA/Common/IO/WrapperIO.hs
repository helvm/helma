module HelVM.HelMA.Common.IO.WrapperIO (
  WrapperIO,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wPutInt,
  wPutIntegral,
  wLogStr,
  wLogStrLn,
  wLogShow,
) where

import qualified System.IO as IO

class Monad m => WrapperIO m where
  wGetChar     :: m Char
  wPutChar     :: Char -> m ()
  wGetLine     :: m String
  wPutStr      :: String -> m ()
  wPutStrLn    :: String -> m ()
  wFlush       :: m ()
  wPutInt      :: Int -> m ()
  wPutIntegral :: Integral v => v -> m ()
  wLogStr      :: String -> m ()
  wLogStrLn    :: String -> m ()
  wLogShow     :: Show s => s -> m ()
  wPutStrLn s  = wPutStr $ s <> "\n"
  wFlush       = pass
  wPutInt      = wPutChar . chr
  wPutIntegral = wPutInt . fromIntegral
  wLogStrLn s  = wLogStr $ s <> "\n"
  wLogShow     = wLogStrLn . show

instance WrapperIO IO where
  wGetChar  = IO.getChar
  wPutChar  = IO.putChar
  wGetLine  = IO.getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = IO.hFlush stdout
  wLogStr   = IO.hPutStr stderr
