module HelVM.HelMA.Automaton.Eff.MockEff (
  ioExecMockEffBatch,
  ioExecMockEffWithInput,

  safeExecMockEffBatch,
  safeExecMockEffWithInput,

  execMockEffBatch,
  execMockEffWithInput,

  runMockEff,
  createMockEff,
  calculateOutput,
  calculateLogged,

  MockEff,
  MockEffData,
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import qualified Data.ByteString.Lazy               as LBS

import           Data.Text                          as Text
import qualified Data.Text.Lazy                     as LT

ioExecMockEffBatch :: ControlT MockEff () -> IO MockEffData
ioExecMockEffBatch = ioExecMockEffWithInput ""

ioExecMockEffWithInput :: Input -> ControlT MockEff () -> IO MockEffData
ioExecMockEffWithInput i = safeToIO . safeExecMockEffWithInput i

safeExecMockEffBatch :: ControlT MockEff () -> Safe MockEffData
safeExecMockEffBatch = safeExecMockEffWithInput ""

safeExecMockEffWithInput :: Input -> ControlT MockEff () -> Safe MockEffData
safeExecMockEffWithInput i = pure . runMockEff i . runControlT

execMockEffBatch :: MockEff () -> MockEffData
execMockEffBatch = execMockEffWithInput ""

execMockEffWithInput :: Input -> MockEff () -> MockEffData
execMockEffWithInput i a = runMockEff i $ safeWithMessages <$> a

----

runMockEff :: Input -> MockEff UnitSafeWithMessages -> MockEffData
runMockEff i mockIO = flip mockDataLogStr mockIOData $ safeWithMessagesToText s
  where (s , mockIOData) = runState mockIO $ createMockEff i

createMockEff :: Input -> MockEffData
createMockEff i = MockEffData (toString i) "" ""

calculateOutput :: MockEffData -> Output
calculateOutput = calculateText . output

calculateLogged :: MockEffData -> Output
calculateLogged = calculateText . logged

----

instance MonadEff MockEff where
  eGetContentsBS   = mockGetContentsBS
  eGetContentsText = mockGetContentsText
  eGetContents     = mockGetContents
  eGetChar         = mockGetChar
  eGetLine         = mockGetLine
  ePutChar         = mockPutChar
  ePutText          = mockPutStr
  eLogText          = mockLogStr

instance MonadEff (SafeT MockEff) where
  eGetContentsBS   = safeT   mockGetContentsBS
  eGetContentsText = safeT   mockGetContentsText
  eGetContents     = safeT   mockGetContents
  eGetChar         = safeT   mockGetChar
  eGetLine         = safeT   mockGetLine
  ePutChar         = safeT . mockPutChar
  ePutText         = safeT . mockPutStr
  eLogText         = safeT . mockLogStr

instance MonadEff (ControlT MockEff) where
  eGetContentsBS   = controlT   mockGetContentsBS
  eGetContentsText = controlT   mockGetContentsText
  eGetContents     = controlT   mockGetContents
  eGetChar         =            mockGetCharSafe
  eGetLine         =            mockGetLineSafe
  ePutChar         = controlT . mockPutChar
  ePutText         = controlT . mockPutStr
  eLogText         = controlT . mockLogStr

----

mockGetContentsBS :: MonadMockEff m => m LBS.ByteString
mockGetContentsBS =  fromStrict . encodeUtf8 <$> mockGetContentsText

mockGetContentsText :: MonadMockEff m => m LT.Text
mockGetContentsText = fromStrict . toText <$> mockGetContents

mockGetContents :: MonadMockEff m => m String
mockGetContents = mockGetContents' =<< get where
  mockGetContents' :: MonadMockEff m => MockEffData -> m String
  mockGetContents' mockIO = content <$ put mockIO { input = "" } where content = input mockIO

mockGetChar :: MonadMockEff m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockEff m => MockEffData -> m Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , Text.show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , Text.show mockIO) $ discard $ input mockIO }

mockGetLine :: MonadMockEff m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockEff m => MockEffData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockGetCharSafe :: MonadControlMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadControlMockEff m => MockEffData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , Text.show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadControlMockEff m => m Text
mockGetLineSafe = mockGetLine' =<< get where
  mockGetLine' :: MonadControlMockEff m => MockEffData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO


mockPutChar :: Char -> MockEff ()
mockPutChar = modify . mockDataPutChar

mockPutStr :: Text -> MockEff ()
mockPutStr = modify . mockDataPutStr

mockLogStr :: Text -> MockEff ()
mockLogStr = modify . mockDataLogStr

----

mockDataPutChar :: Char -> MockEffData -> MockEffData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutStr :: Text -> MockEffData -> MockEffData
mockDataPutStr text mockIO = mockIO { output = calculateString text <> output mockIO }

mockDataLogStr :: Text -> MockEffData -> MockEffData
mockDataLogStr text mockIO = mockIO { logged = calculateString text <> logged mockIO }

----

type MonadControlMockEff m = (MonadMockEff m , MonadControl m)--FIXME

--type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m) --FIXME

type MonadMockEff m = MonadState MockEffData m

type MockEff = State MockEffData

calculateText :: String -> Output
calculateText = Text.reverse . toText

calculateString :: Output -> String
calculateString =  toString . Text.reverse

data MockEffData = MockEffData
  { input  :: !String
  , output :: !String
  , logged :: !String
  }
  deriving stock (Eq , Read , Show)

----

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
