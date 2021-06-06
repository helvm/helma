module HelVM.HelMA.Common.IO.MockIO (
  batchExecMockIO,
  flipExecMockIO,
  execMockIO,
  batchEvalMockIO,
  flipEvalMockIO,
  evalMockIO,
  getLogged,
  MockIO
) where

import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Util

import qualified Relude.Unsafe as Unsafe

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flipExecMockIO []

flipExecMockIO :: Input -> MockIO () -> Output
flipExecMockIO = flip execMockIO

execMockIO :: MockIO () -> Interact
execMockIO mockIO  = getOutput . execState mockIO . createMockIO

batchEvalMockIO :: MockIO () -> Output
batchEvalMockIO = flipEvalMockIO []

flipEvalMockIO :: Input -> MockIO () -> Output
flipEvalMockIO = flip evalMockIO

evalMockIO :: MockIO () -> Interact
evalMockIO mockIO = getLogged . execState mockIO . createMockIO

----

instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

mockGetChar :: MockIO Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' mockIO = headOrError mockIO (input mockIO) <$ put mockIO { input = Unsafe.tail $ input mockIO }

mockGetLine :: MockIO String
mockGetLine = mockGetLine' =<< get where
  mockGetLine' mockIO = line <$ put mockIO { input = input' } where (line , input') = splitStringByEndLine $ input mockIO

mockPutChar :: Char -> MockIO ()
mockPutChar char = mockPutChar' =<< get where
  mockPutChar' mockIO = put mockIO { output = char : output mockIO }

mockPutInt :: Int -> MockIO ()
mockPutInt value = mockPutInt' =<< get where
  mockPutInt' mockIO = put $ mockIO { output = chr  value : output mockIO }


mockPutStr :: String -> MockIO ()
mockPutStr string = mockPutStr' =<< get where
  mockPutStr' mockIO = put $ mockIO { output = reverse string <> output mockIO }


mockLogStr :: String -> MockIO ()
mockLogStr string = mockLogStr' =<< get where
  mockLogStr' mockIO = put $ mockIO { logged = reverse string <> logged mockIO }

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
  deriving (Eq , Show , Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x:_) =  x
headOrError e []    =  error $ show e
