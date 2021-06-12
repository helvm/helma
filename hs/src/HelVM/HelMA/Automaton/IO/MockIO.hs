module HelVM.HelMA.Automaton.IO.MockIO (
  batchExecMockIO,
  flipExecMockIO,
  execMockIO,
  batchEvalMockIO,
  flipEvalMockIO,
  evalMockIO,
  getLogged,
  MockIO
) where

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.WrapperIO

import HelVM.Common.Containers.SplitAt

import qualified Relude.Unsafe as Unsafe

batchExecMockIO :: MockIO_ -> Output
batchExecMockIO = flipExecMockIO ""

flipExecMockIO :: Input -> MockIO_ -> Output
flipExecMockIO = flip execMockIO

execMockIO :: MockIO () -> Interact
execMockIO mockIO  = getOutput . execState mockIO . createMockIO

batchEvalMockIO :: MockIO_ -> Output
batchEvalMockIO = flipEvalMockIO ""

flipEvalMockIO :: Input -> MockIO_ -> Output
flipEvalMockIO = flip evalMockIO

evalMockIO :: MockIO_ -> Interact
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

mockGetLine :: MockIO Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: Char -> MockIO_
mockPutChar char = mockPutChar' =<< get where
  mockPutChar' mockIO = put mockIO { output = char : output mockIO }

mockPutInt :: Int -> MockIO_
mockPutInt value = mockPutInt' =<< get where
  mockPutInt' mockIO = put $ mockIO { output = chr  value : output mockIO }


mockPutStr :: Text -> MockIO_
mockPutStr text = mockPutStr' =<< get where
  mockPutStr' mockIO = put $ mockIO { output = reverse (toString text) <> output mockIO }


mockLogStr :: Text -> MockIO_
mockLogStr text = mockLogStr' =<< get where
  mockLogStr' mockIO = put $ mockIO { logged = reverse (toString text) <> logged mockIO }

----

type MockIO_ = MockIO ()

type MockIO = State MockIOData

createMockIO :: Input -> MockIOData
createMockIO i = MockIOData (toString i) "" ""

getOutput :: MockIOData -> Output
getOutput (MockIOData _ o _) = toText $ reverse o

getLogged :: MockIOData -> Output
getLogged (MockIOData _ _ e) = toText $ reverse e

data MockIOData = MockIOData
  { input  :: String
  , output :: String
  , logged  :: String
  }
  deriving stock (Eq , Show , Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x:_) =  x
headOrError e []    =  error $ show e

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
