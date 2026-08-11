module HelVM.HelMA.Automata.Piet.API.LexerType where

import           Data.Default

defaultLexerType ∷ LexerType
defaultLexerType = def

fileLexerTypes ∷ NonEmpty LexerType
fileLexerTypes = universeNonEmpty

data LexerType = JuicyPixels | NetPBM
  deriving stock (Bounded , Enum , Eq , Read , Show)

instance Default LexerType where
  def = minBound
