module HelVM.HelCam.Common.FilterIf0 where

import HelVM.HelCam.Common.WrapperIO

import qualified System.IO as IO

pipe :: IO ()
pipe = do
  char <- IO.getChar
  IO.putChar char
  pipe

filterIf0 :: IO ()
filterIf0 = do
  char <- IO.getChar
  if char == '0'
    then IO.putChar '\n'
    else do
      IO.putChar char
      filterIf0

listFilterIf0 :: String -> String
listFilterIf0 []          = []
listFilterIf0 (char:rest) =
  if char == '0'
    then ['\n']
    else char : listFilterIf0 rest

----

-- getChar :: IO Char
-- putChar :: Char -> IO ()

type IOGetChar = IO Char
type IOPutChar = Char -> IO ()

ioFilterIf0 :: IO ()
ioFilterIf0 = ioFilterIf0' IO.getChar IO.putChar

ioFilterIf0' :: IOGetChar -> IOPutChar -> IO ()
ioFilterIf0' ioGetChar ioPutChar = do
  char <- ioGetChar
  if char == '0'
    then ioPutChar '\n'
    else do
      ioPutChar char
      ioFilterIf0' ioGetChar ioPutChar

----

mFilterIf0 :: Monad m => MGetChar m -> MPutChar m -> m ()
mFilterIf0 mGetChar mPutChar = do
  char <- mGetChar
  if char == '0'
    then mPutChar '\n'
    else do
      mPutChar char
      mFilterIf0 mGetChar mPutChar

ioMFilterIf0 :: IO ()
ioMFilterIf0 = mFilterIf0 IO.getChar IO.putChar

----

wFilterIf0 :: WrapperIO m => m ()
wFilterIf0 = do
  char <- wGetChar
  if char == '0'
    then wPutChar '\n'
    else do
      wPutChar char
      wFilterIf0

ioWFilterIf0 :: IO ()
ioWFilterIf0 = wFilterIf0

----

iFilterIf0 :: IO ()
iFilterIf0 = IO.interact listFilterIf0
