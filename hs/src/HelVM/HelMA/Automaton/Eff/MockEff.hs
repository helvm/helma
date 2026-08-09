module HelVM.HelMA.Automaton.Eff.MockEff (
  mockGetContentsBS,
  mockGetContentsText,
  mockGetContents,
  mockGetChar,
  mockGetLine,
  mockGetCharSafe,
  mockGetLineSafe,
  mockPutChar,
  mockPutText,

  createMockEffData,
  reverseOutput,

  MonadMockEff,
  MonadSafeMockEff,
  MockEffData (..),
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
  mockGetContents' :: MonadMockEff m => MockEffData -> m String
  mockGetContents' mockEff = content <$ put mockEff { input = "" } where content = input mockEff

mockGetChar :: MonadMockEff m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockEff m => MockEffData -> m Char
  mockGetChar' mockEff = orErrorTuple ("mockGetChar" , Text.show mockEff) (top (input mockEff)) <$ put mockEff { input = orErrorTuple ("mockGetChar" , Text.show mockEff) $ discard $ input mockEff }

mockGetLine :: MonadMockEff m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockEff m => MockEffData -> m Text
  mockGetLine' mockEff = toText line <$ put mockEff { input = input' } where (line , input') = splitStringByLn $ input mockEff

mockGetCharSafe :: MonadSafeMockEff m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadSafeMockEff m => MockEffData -> m Char
  mockGetChar' mockEff = appendErrorTuple ("mockGetCharSafe" , Text.show mockEff) $ mockGetChar'' =<< unconsSafe (input mockEff) where
    mockGetChar'' (c, input') = put mockEff { input = input' } $> c

mockGetLineSafe :: MonadSafeMockEff m => m Text
mockGetLineSafe = mockGetLineSafe' =<< get where
  mockGetLineSafe' :: MonadSafeMockEff m => MockEffData -> m Text
  mockGetLineSafe' mockEff = toText line <$ put mockEff { input = input' } where (line , input') = splitStringByLn $ input mockEff

mockPutChar :: MonadMockEff m => Char -> m ()
mockPutChar = modify . mockDataPutChar

mockPutText :: MonadMockEff m => Text -> m ()
mockPutText = modify . mockDataPutText

----

createMockEffData :: Input -> MockEffData
createMockEffData = MockEffData "" . toString

reverseOutput :: MockEffData -> Output
reverseOutput = reverseText . output

reverseText :: String -> Output
reverseText = S.reverse . toText

reverseString :: Output -> String
reverseString = toString . S.reverse

mockDataPutChar :: Char -> MockEffData -> MockEffData
mockDataPutChar char mockEff = mockEff { output = char : output mockEff }

mockDataPutText :: Text -> MockEffData -> MockEffData
mockDataPutText text mockEff = mockEff { output = reverseString text <> output mockEff }

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'

----

type MonadSafeMockEff m = (MonadSafe m , MonadMockEff m)
type MonadMockEff m = MonadState MockEffData m

data MockEffData = MockEffData
  { output :: !String
  , input  :: !String
  }
  deriving stock (Eq , Show)
