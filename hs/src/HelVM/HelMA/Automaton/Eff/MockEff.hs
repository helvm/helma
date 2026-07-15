{-# LANGUAGE UndecidableInstances #-}
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
import           HelVM.HelMA.Automaton.API.LogLevel

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import qualified Data.DList                         as DList
import           Data.Text                          as Text

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
runMockEff i mockIO = flip mockDataLogInfo mockIOData $ safeWithMessagesToText s
  where (s , mockIOData) = runState mockIO $ createMockEff i

createMockEff :: Input -> MockEffData
createMockEff i = MockEffData  (toString i) "" DList.empty

calculateOutput :: MockEffData -> Output
calculateOutput = calculateText . output

calculateLogged :: MockEffData -> Output
calculateLogged d = Text.concat $ DList.toList $ snd <$> logged d

----

instance MonadEff MockEff where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getContents     = mockGetContents
  eGetChar         = mockGetChar
  eGetLine         = mockGetLine
  ePutChar         = mockPutChar
  ePutText         = mockPutText
  log              = mockLog

instance MonadEff (SafeT MockEff) where
  getContentsBS   = mockGetContentsBS
  getContentsText = mockGetContentsText
  getContents     = mockGetContents
  eGetChar         = mockGetCharSafe
  eGetLine         = mockGetLineSafe
  ePutChar         = mockPutChar
  ePutText         = mockPutText
  log              = mockLog

----

mockGetContentsBS :: MonadMockEff m => m LByteString
mockGetContentsBS =  fromStrict . encodeUtf8 <$> mockGetContentsText

mockGetContentsText :: MonadMockEff m => m LText
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

mockGetCharSafe :: MonadSafeMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadSafeMockEff m => MockEffData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , Text.show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadSafeMockEff m => m Text
mockGetLineSafe = mockGetLine' =<< get where
  mockGetLine' :: MonadSafeMockEff m => MockEffData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO


mockPutChar :: MonadMockEff m => Char -> m ()
mockPutChar = modify . mockDataPutChar

mockPutText :: MonadMockEff m => Text -> m ()
mockPutText = modify . mockDataPutText

mockLog :: MonadMockEff m => LogLevel -> Text -> m ()
mockLog l m = modify $ mockDataLog l m

----

mockDataPutChar :: Char -> MockEffData -> MockEffData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutText :: Text -> MockEffData -> MockEffData
mockDataPutText text mockIO = mockIO { output = calculateString text <> output mockIO }

mockDataLogInfo :: Text -> MockEffData -> MockEffData
mockDataLogInfo = mockDataLog Info

mockDataLog :: LogLevel -> Text -> MockEffData -> MockEffData
mockDataLog l m mockIO = mockIO { logged = logged mockIO <> DList.singleton (l , m) }

----

-- type MonadControlMockEff m = (MonadMockEff m , MonadControl m)--FIXME

type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m) --FIXME

type MonadMockEff m = MonadState MockEffData m

type MockEff = State MockEffData

calculateText :: String -> Output
calculateText = Text.reverse . toText

calculateString :: Output -> String
calculateString =  toString . Text.reverse

data MockEffData = MockEffData
  { input  :: !String
  , output :: !String
  , logged :: !Logs
  }
  deriving stock (Eq , Read , Show)

----

type Logs = DList.DList(LogLevel, Text)

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
