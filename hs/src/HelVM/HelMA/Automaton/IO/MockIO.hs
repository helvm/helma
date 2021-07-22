module HelVM.HelMA.Automaton.IO.MockIO (
  ioExecMockIOBatch,
  ioExecMockIOWithInput,
  safeExecMockIOBatch,
  safeExecMockIOWithInput,
  execMockIOBatch,
  execMockIOWithInput,

  execMockIO,
  createMockIO,
  calculateOutput,
  calculateLogged,

  MockIO,
  MockIOData,
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.ListLikeUtil
import           HelVM.Common.Safe

import           Data.Text                           as Text

ioExecMockIOBatch :: SafeExceptT MockIO () -> IO MockIOData
ioExecMockIOBatch = ioExecMockIOWithInput ""

ioExecMockIOWithInput :: Input -> SafeExceptT MockIO () -> IO MockIOData
ioExecMockIOWithInput i = safeToIO . pure . execMockIOWithInput i . runExceptT

safeExecMockIOBatch ::  SafeExceptT MockIO () -> Safe MockIOData
safeExecMockIOBatch = safeExecMockIOWithInput ""

safeExecMockIOWithInput :: Input -> SafeExceptT MockIO () -> Safe MockIOData
safeExecMockIOWithInput i = pure . execMockIOWithInput i . runExceptT

execMockIOBatch :: MockIO a -> MockIOData
execMockIOBatch = execMockIOWithInput ""

execMockIOWithInput :: Input -> MockIO a -> MockIOData
execMockIOWithInput = flip execMockIO

----

execMockIO :: MockIO a -> Input -> MockIOData
execMockIO mockIO = execState mockIO . createMockIO

createMockIO :: Input -> MockIOData
createMockIO i = MockIOData (toString i) "" ""

calculateOutput :: MockIOData -> Output
calculateOutput = calculateText . output

calculateLogged :: MockIOData -> Output
calculateLogged = calculateText . logged

----

instance BusinessIO MockIO where
  wGetChar = mockGetChar
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

instance BusinessIO (SafeExceptT MockIO) where
  wGetChar = safeExceptT   mockGetChar
  wGetLine = safeExceptT   mockGetLine
  wPutChar = safeExceptT . mockPutChar
  wPutInt  = safeExceptT . mockPutInt
  wPutStr  = safeExceptT . mockPutStr
  wLogStr  = safeExceptT . mockLogStr

----

mockGetChar :: MockIO Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadState MockIOData f => MockIOData -> f Char
  mockGetChar' mockIO = headOrError mockIO (input mockIO) <$ put mockIO { input = tailOrError mockIO $ input mockIO }

mockGetLine :: MockIO Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadState MockIOData f => MockIOData -> f Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: Char -> MockIO ()
mockPutChar char = mockPutChar' =<< get where
  mockPutChar' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutChar' mockIO = put mockIO { output = char : output mockIO }

mockPutInt :: Int -> MockIO ()
mockPutInt value = mockPutInt' =<< get where
  mockPutInt' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutInt' mockIO = put $ mockIO { output = chr  value : output mockIO }

mockPutStr :: Text -> MockIO ()
mockPutStr text = mockPutStr' =<< get where
  mockPutStr' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutStr' mockIO = put $ mockIO { output = calculateString text <> output mockIO }

mockLogStr :: Text -> MockIO ()
mockLogStr text = mockLogStr' =<< get where
  mockLogStr' :: MonadState MockIOData f => MockIOData -> f ()
  mockLogStr' mockIO = put $ mockIO { logged = calculateString text <> logged mockIO }

----

type MockIO = State MockIOData

calculateText :: String -> Output
calculateText = Text.reverse . toText

calculateString :: Output -> String
calculateString =  toString . Text.reverse

data MockIOData = MockIOData
  { input  :: !String
  , output :: !String
  , logged :: !String
  }
  deriving stock (Eq , Show , Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x : _) =  x
headOrError e []      =  error $ show e

tailOrError :: Show e => e -> [a] -> [a]
tailOrError _ (_ : xs) =  xs
tailOrError e []       =  error $ show e

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
