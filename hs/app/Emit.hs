module Emit where

defaultEmit :: Emit
defaultEmit = minBound

emits :: NonEmpty Emit
emits = universeNonEmpty

data Emit = No | IL | TL | Code
  deriving stock (Bounded , Enum , Eq , Read , Show)
