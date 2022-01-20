module HelVM.HelMA.Automaton.IO.MockIO (
  ioExecMockIOBatch,
  ioExecMockIOWithInput,

  safeExecMockIOBatch,
  safeExecMockIOWithInput,

  execMockIOBatch,
  execMockIOWithInput,

  runMockIO,
  createMockIO,
  calculateOutput,
  calculateLogged,

  MockIO,
  MockIOData,
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.Control.Control
import           HelVM.Common.Control.Safe

import           HelVM.Common.ListLikeUtil

import           Data.Text                           as Text

ioExecMockIOBatch :: ControlT MockIO () -> IO MockIOData
ioExecMockIOBatch = ioExecMockIOWithInput ""

ioExecMockIOWithInput :: Input -> ControlT MockIO () -> IO MockIOData
ioExecMockIOWithInput i = safeToIO . safeExecMockIOWithInput i

safeExecMockIOBatch :: ControlT MockIO () -> Safe MockIOData
safeExecMockIOBatch = safeExecMockIOWithInput ""

safeExecMockIOWithInput :: Input -> ControlT MockIO () -> Safe MockIOData
safeExecMockIOWithInput i = pure . runMockIO i . runControlT

execMockIOBatch :: MockIO () -> MockIOData
execMockIOBatch = execMockIOWithInput ""

execMockIOWithInput :: Input -> MockIO () -> MockIOData
execMockIOWithInput i a = runMockIO i $ safeWithMessages <$> a

----

runMockIO :: Input -> MockIO UnitSafeWithMessages -> MockIOData
runMockIO i mockIO = flip mockDataLogStr mockIOData $ safeWithMessagesToText s
  where (s , mockIOData) = runState mockIO $ createMockIO i

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
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

instance BusinessIO (SafeT MockIO) where
  wGetChar = safeT   mockGetChar
  wGetLine = safeT   mockGetLine
  wPutChar = safeT . mockPutChar
  wPutStr  = safeT . mockPutStr
  wLogStr  = safeT . mockLogStr

instance BusinessIO (ControlT MockIO) where
  wGetChar = controlT   mockGetChar
  wGetLine = controlT   mockGetLine
  wPutChar = controlT . mockPutChar
  wPutStr  = controlT . mockPutStr
  wLogStr  = controlT . mockLogStr

----

mockGetChar :: MockIO Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadState MockIOData f => MockIOData -> f Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , show mockIO) $ discard $ input mockIO }

mockGetLine :: MockIO Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadState MockIOData f => MockIOData -> f Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: Char -> MockIO ()
mockPutChar = modify . mockDataPutChar

mockPutStr :: Text -> MockIO ()
mockPutStr = modify . mockDataPutStr

mockLogStr :: Text -> MockIO ()
mockLogStr = modify . mockDataLogStr

----

mockDataPutChar :: Char -> MockIOData -> MockIOData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutStr :: Text -> MockIOData -> MockIOData
mockDataPutStr text mockIO = mockIO { output = calculateString text <> output mockIO }

mockDataLogStr :: Text -> MockIOData -> MockIOData
mockDataLogStr text mockIO = mockIO { logged = calculateString text <> logged mockIO }

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

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
