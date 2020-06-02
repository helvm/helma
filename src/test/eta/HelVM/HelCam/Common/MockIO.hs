{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelCam.Common.MockIO (batchExecMockIO, execMockIO, mockGetChar, mockPutChar, mockGetLine, mockPutStr) where

import HelVM.HelCam.Common.WrapperIO
import HelVM.HelCam.Common.Util

import Control.Monad.State.Lazy

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flip execMockIO []

execMockIO :: MockIO () -> Interact
execMockIO mockIO input = getOutput $ execState mockIO $ createMockIO input

----

mockGetChar :: MockIO Char
mockGetChar = do
  state <- get
  let char = head $ input state
  put state { input = tail $ input state }
  return char

mockPutChar :: Char -> MockIO ()
mockPutChar char = do
  state <- get
  put state { output = char : output state }

mockGetLine :: MockIO String
mockGetLine = do
  state <- get
  let pair = splitStringByEndLine (input state)
  put state { input = snd pair }
  return $ fst pair

mockPutStr :: String -> MockIO ()
mockPutStr string = do
  state <- get
  put $ state { output = reverse string ++ output state }

instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wPutChar = mockPutChar
  wGetLine = mockGetLine
  wPutStr  = mockPutStr

----

type MockIO = State MockIOData

getOutput :: MockIOData -> String
getOutput (MockIOData input output) = reverse output

createMockIO :: String -> MockIOData
createMockIO = flip MockIOData []

data MockIOData = MockIOData
  { input :: String
  , output :: String
  }
  deriving (Eq, Show, Read)
