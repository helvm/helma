module HelVM.HelMA.Automaton.Eff.MockIO (
  createMockIOData,
  mockDataPutChar,
  mockDataPutText,
  reverseOutput,
  splitStringByLn,

  MonadSafeMockEff,
  MonadMockEff,
  MockIOData (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ListLikeExtra

import qualified Data.Sequences                    as S

createMockIOData :: Input -> MockIOData
createMockIOData = MockIOData "" . toString

mockDataPutChar :: Char -> MockIOData -> MockIOData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutText :: Text -> MockIOData -> MockIOData
mockDataPutText text mockIO = mockIO { output = calculateString text <> output mockIO }

reverseOutput :: MockIOData -> Output
reverseOutput = reverseText . output

reverseText :: String -> Output
reverseText = S.reverse . toText

calculateString :: Output -> String
calculateString = toString . S.reverse

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'

type MonadSafeMockEff m = (MonadMockEff m , MonadSafe m)
type MonadMockEff m = MonadState MockIOData m

data MockIOData = MockIOData
  { output :: !String
  , input  :: !String
  }
  deriving stock (Eq , Show)
