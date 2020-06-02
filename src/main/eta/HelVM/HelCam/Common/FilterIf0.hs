module HelVM.HelCam.Common.FilterIf0 where

import HelVM.HelCam.Common.WrapperIO

pipe :: IO ()
pipe = do
  char <- getChar
  putChar char
  pipe

filterIf0 :: IO ()
filterIf0 = do
  char <- getChar
  if char == '0'
    then putChar '\n'
    else do
      putChar char
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
ioFilterIf0 = ioFilterIf0' getChar putChar

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
ioMFilterIf0 = mFilterIf0 getChar putChar

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
iFilterIf0 = interact listFilterIf0
