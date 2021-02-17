{-# LANGUAGE FlexibleInstances #-}

module HelVM.HelCam.Common.MockIO (
  batchExecMockIO, execMockIO, 
  batchEvalMockIO, evalMockIO,
  getLogged,
  MockIO
) where

import HelVM.HelCam.Common.WrapperIO
import HelVM.HelCam.Common.Util

import qualified Relude.Unsafe as Unsafe

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flip execMockIO []

execMockIO :: MockIO () -> Interact
execMockIO mockIO i = getOutput $ execState mockIO $ createMockIO i

batchEvalMockIO :: MockIO () -> Output
batchEvalMockIO = flip evalMockIO []

evalMockIO :: MockIO () -> Interact
evalMockIO mockIO i = getLogged $ execState mockIO $ createMockIO i

----

instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wGetInt  = mockGetInt
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

mockGetChar :: MockIO Char
mockGetChar = do
  mockIO <- get
  let char = headOrError mockIO $ input mockIO
  put mockIO { input = Unsafe.tail $ input mockIO }
  return char

mockGetInt :: MockIO Int
mockGetInt = do ord <$> mockGetChar

mockGetLine :: MockIO String
mockGetLine = do
  mockIO <- get
  let pair = splitStringByEndLine (input mockIO)
  put mockIO { input = snd pair }
  return $ fst pair

mockPutChar :: Char -> MockIO ()
mockPutChar char = do
  mockIO <- get
  put mockIO { output = char : output mockIO }

mockPutInt :: Integral i => i -> MockIO ()
mockPutInt value = do
  mockIO <- get
  put $ mockIO { output = chr (fromIntegral value) : output mockIO }


mockPutStr :: String -> MockIO ()
mockPutStr string = do
  mockIO <- get
  put $ mockIO { output = reverse string <> output mockIO }


mockLogStr :: String -> MockIO ()
mockLogStr string = do
  mockIO <- get
  put $ mockIO { logged = reverse string <> logged mockIO }

----

type MockIO = State MockIOData

createMockIO :: String -> MockIOData
createMockIO i = MockIOData i [] []

getOutput :: MockIOData -> String
getOutput (MockIOData _ o _) = reverse o

getLogged :: MockIOData -> String
getLogged (MockIOData _ _ e) = reverse e

data MockIOData = MockIOData
  { input  :: String
  , output :: String
  , logged  :: String
  }
  deriving (Eq, Show, Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x:_) =  x
headOrError e []    =  error $ show e
