module HelVM.HelCam.Common.WrapperIO where

import qualified System.IO as IO

type MGetChar m = m Char
type MPutChar m = Char -> m ()

type MGetLine m = m String
type MPutStr m = String -> m ()

type MChar m = (MGetChar m, MPutChar m)

type MString m = (MGetChar m, MPutChar m, MGetLine m, MPutStr m)

--

class Monad m => WrapperIO m where
  wGetChar  :: m Char
  wPutChar  :: Char -> m ()
  wGetLine  :: m String
  wPutStr   :: String -> m ()
  wPutStrLn :: String -> m ()
  wFlush    :: m ()
  wGetInt   :: m Int
  wPutInt   :: Integral i => i -> m ()
  wLogStr   :: String -> m ()
  wLogStrLn   :: String -> m ()
  wPutStrLn s = wPutStr $ s <> "\n"
  wFlush = pass
  wPutInt value = wPutChar (chr (fromIntegral value))
  wGetInt = do ord <$> wGetChar
  wLogStrLn s = wLogStr $ s <> "\n"

instance WrapperIO IO where
  wGetChar  = IO.getChar
  wPutChar  = IO.putChar
  wGetLine  = IO.getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = IO.hFlush stdout
  wLogStr   = IO.hPutStr stderr
