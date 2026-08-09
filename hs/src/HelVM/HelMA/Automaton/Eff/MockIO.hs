module HelVM.HelMA.Automaton.Eff.MockIO (
  createMockIOData,
  mockDataPutChar,
  mockDataPutText,
  calculateOutput,
  splitStringByLn,

  MockIOData (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.ListLikeExtra

import qualified Data.Sequences                    as S

createMockIOData :: Input -> MockIOData
createMockIOData = MockIOData "" . toString

mockDataPutChar :: Char -> MockIOData -> MockIOData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutText :: Text -> MockIOData -> MockIOData
mockDataPutText text mockIO = mockIO { output = calculateString text <> output mockIO }

calculateOutput :: MockIOData -> Output
calculateOutput = calculateText . output

calculateText :: String -> Output
calculateText = S.reverse . toText

calculateString :: Output -> String
calculateString = toString . S.reverse

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'

data MockIOData = MockIOData
  { output :: !String
  , input  :: !String
  }
  deriving stock (Eq , Show)
