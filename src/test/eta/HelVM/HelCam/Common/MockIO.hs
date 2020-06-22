{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelCam.Common.MockIO (batchExecMockIO, execMockIO, mockGetChar, mockPutChar, mockGetLine, mockPutStr) where

import HelVM.HelCam.Common.WrapperIO
import HelVM.HelCam.Common.Util

import Control.Monad.State.Lazy

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flip execMockIO []

execMockIO :: MockIO () -> Interact
execMockIO mockIO i = getOutput $ execState mockIO $ createMockIO i

----

mockGetChar :: MockIO Char
mockGetChar = do
  mockIO <- get
  let char = head $ input mockIO
  put mockIO { input = tail $ input mockIO }
  return char

mockPutChar :: Char -> MockIO ()
mockPutChar char = do
  mockIO <- get
  put mockIO { output = char : output mockIO }

mockGetLine :: MockIO String
mockGetLine = do
  mockIO <- get
  let pair = splitStringByEndLine (input mockIO)
  put mockIO { input = snd pair }
  return $ fst pair

mockPutStr :: String -> MockIO ()
mockPutStr string = do
  mockIO <- get
  put $ mockIO { output = reverse string ++ output mockIO }

instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wPutChar = mockPutChar
  wGetLine = mockGetLine
  wPutStr  = mockPutStr

----

type MockIO = State MockIOData

getOutput :: MockIOData -> String
getOutput (MockIOData _ o) = reverse o

createMockIO :: String -> MockIOData
createMockIO = flip MockIOData []

data MockIOData = MockIOData
  { input :: String
  , output :: String
  }
  deriving (Eq, Show, Read)
