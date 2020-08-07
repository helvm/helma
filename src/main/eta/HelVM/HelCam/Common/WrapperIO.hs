module HelVM.HelCam.Common.WrapperIO where

import Data.Char

import System.IO

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
  wPutStrLn s = wPutStr $ s ++ "\n"
  wFlush = return ()
  wPutInt value = wPutChar (chr (fromIntegral value))
  wGetInt = do ord <$> wGetChar
  wLogStrLn s = wLogStr $ s ++ "\n"

instance WrapperIO IO where
  wGetChar  = getChar
  wPutChar  = putChar
  wGetLine  = getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = hFlush stdout
  wLogStr   = hPutStr stderr
