module HelVM.HelMA.Common.IO.WrapperIO (
  WrapperIO,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wGetInt,
  wPutInt,
  wLogStr,
  wLogStrLn,
  wLogShow,
) where

import qualified System.IO as IO

class Monad m => WrapperIO m where
  wGetChar  :: m Char
  wPutChar  :: Char -> m ()
  wGetLine  :: m String
  wPutStr   :: String -> m ()
  wPutStrLn :: String -> m ()
  wFlush    :: m ()
  wGetInt   :: m Int
  wPutInt   :: Int -> m ()
  wLogStr   :: String -> m ()
  wLogStrLn :: String -> m ()
  wLogShow  :: Show s => s -> m ()
  wPutStrLn s = wPutStr $ s <> "\n"
  wFlush      = pass
  wPutInt     = wPutChar . chr
  wGetInt     = ord <$> wGetChar
  wLogStrLn s = wLogStr $ s <> "\n"
  wLogShow  s = wLogStrLn $ show s

instance WrapperIO IO where
  wGetChar  = IO.getChar
  wPutChar  = IO.putChar
  wGetLine  = IO.getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = IO.hFlush stdout
  wLogStr   = IO.hPutStr stderr
