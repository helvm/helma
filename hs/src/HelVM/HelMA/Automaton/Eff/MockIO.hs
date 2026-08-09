module HelVM.HelMA.Automaton.Eff.MockIO (
  mockGetContentsBS,
  mockGetContentsText,
  mockGetContents,
  mockGetChar,
  mockGetLine,
  mockGetCharSafe,
  mockGetLineSafe,
  mockPutChar,
  mockPutText,

  createMockIOData,
  reverseOutput,

  MonadMockEff,
  MonadSafeMockEff,
  MockIOData (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import qualified Data.Sequences                    as S
import qualified Data.Text                         as Text

mockGetContentsBS :: MonadMockEff m => m LByteString
mockGetContentsBS = fromStrict . encodeUtf8 <$> mockGetContentsText

mockGetContentsText :: MonadMockEff m => m LText
mockGetContentsText = fromStrict . toText <$> mockGetContents

mockGetContents :: MonadMockEff m => m String
mockGetContents = mockGetContents' =<< get where
  mockGetContents' :: MonadMockEff m => MockIOData -> m String
  mockGetContents' mockIO = content <$ put mockIO { input = "" } where content = input mockIO

mockGetChar :: MonadMockEff m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockEff m => MockIOData -> m Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , Text.show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , Text.show mockIO) $ discard $ input mockIO }

mockGetLine :: MonadMockEff m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockEff m => MockIOData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockGetCharSafe :: MonadSafeMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadSafeMockEff m => MockIOData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , Text.show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadSafeMockEff m => m Text
mockGetLineSafe = mockGetLineSafe' =<< get where
  mockGetLineSafe' :: MonadSafeMockEff m => MockIOData -> m Text
  mockGetLineSafe' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: MonadMockEff m => Char -> m ()
mockPutChar = modify . mockDataPutChar

mockPutText :: MonadMockEff m => Text -> m ()
mockPutText = modify . mockDataPutText

----

createMockIOData :: Input -> MockIOData
createMockIOData = MockIOData "" . toString

reverseOutput :: MockIOData -> Output
reverseOutput = reverseText . output

reverseText :: String -> Output
reverseText = S.reverse . toText

reverseString :: Output -> String
reverseString = toString . S.reverse

mockDataPutChar :: Char -> MockIOData -> MockIOData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutText :: Text -> MockIOData -> MockIOData
mockDataPutText text mockIO = mockIO { output = reverseString text <> output mockIO }

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'

----

type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m)
type MonadMockEff m = MonadState MockIOData m

data MockIOData = MockIOData
  { output :: !String
  , input  :: !String
  }
  deriving stock (Eq , Show)
