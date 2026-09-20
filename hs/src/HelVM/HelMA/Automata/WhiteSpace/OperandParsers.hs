module HelVM.HelMA.Automata.WhiteSpace.OperandParsers where

import           HelVM.HelMA.Automata.WhiteSpace.Token
import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction

import           HelVM.HelMA.Automaton.API.LabelType

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Digit.ToDigit

import           Control.Applicative.Tools
import           Control.Monad.Extra

parseIndex ∷ MonadSafe m ⇒ ParserFromTokenList m ImmediateIndex
parseIndex = parseInt

parseSymbol ∷ MonadSafe m ⇒ ParserFromTokenList m Symbol
parseSymbol = parseInteger

parseLabel ∷ MonadSafe m ⇒ LabelType → ParserFromTokenList m Label
parseLabel BinaryLabel = parseDigitString
parseLabel TextLabel   = parseAsciiString

----

parseInt ∷ MonadSafe m ⇒ ParserFromTokenList m Int
parseInt tl = parseInt' <$> parseInteger tl where
  parseInt' (integer , tl') = (fromIntegral integer , tl')

parseInteger ∷ MonadSafe m ⇒ ParserFromTokenList m Integer
parseInteger []       = liftError "EOL"
parseInteger (S : tl) = parseExtra makeIntegral2FromList tl
parseInteger (T : tl) = negationIntegral <$> parseExtra makeIntegral2FromList tl
parseInteger (N : tl) = pure (0 , tl)

negationIntegral ∷ (Integer , TokenList) → (Integer , TokenList)
negationIntegral (i , l) = (-i , l)

parseNatural ∷ MonadSafe m ⇒ ParserFromTokenList m Natural
parseNatural = parseExtra makeIntegral2FromList

parseExtra ∷ MonadSafe m ⇒ (TokenList → m a) → ParserFromTokenList m a
parseExtra maker = loop act . ([] , ) where
  act (acc ,      []) = Right $ liftError $ show acc
  act (acc ,  N : tl) = Right $ moveSafe (maker acc , tl)
  act (acc ,  t : tl) = Left (t : acc , tl)

parseDigitString ∷ MonadSafe m ⇒ ParserFromTokenList m Text
parseDigitString tl = moveSafe =<< parseString' makeDigitStringFromList' tl

parseAsciiString ∷ MonadSafe m ⇒ ParserFromTokenList m Text
parseAsciiString tl = moveSafe =<< parseString' makeAsciiString28FromList' tl

makeDigitStringFromList' ∷ MonadSafe m ⇒ TokenList → m Text
makeDigitStringFromList'  = (toText . toList) <.> makeDigitStringFromList

makeAsciiString28FromList' ∷ MonadSafe m ⇒ TokenList → m Text
makeAsciiString28FromList' = (toText . toList) <.> makeAsciiString28FromList

moveSafe ∷ MonadSafe m ⇒ (m a , TokenList) → m (a , TokenList)
moveSafe (a , tl) = appendErrorTuple ("TokenList" , show tl) $ ( , tl) <$> a

parseString' ∷ MonadSafe m ⇒ (TokenList → a) → ParserFromTokenList m a
parseString' maker tl = parseString'' <$> splitByN tl where
  parseString'' (acc , tl') = (maker acc , tl')

splitByN ∷ MonadSafe m ⇒ ParserFromTokenList m TokenList
splitByN []       = liftError "Empty list"
splitByN (N : tl) = pure ([]    , tl)
splitByN (t : tl) = splitByN' <$> splitByN tl where
  splitByN' (acc , tl') = (t:acc , tl')

-- | Types
type ParserFromTokenList m a = Parser TokenList m a

type Parser b m a = b → m (a , b)
