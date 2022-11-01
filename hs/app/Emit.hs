module Emit where

import           HelVM.HelIO.SwitchEnum

defaultEmit :: Emit
defaultEmit = defaultEnum

emits :: [Emit]
emits = generateEnums 4

data Emit = No | IL | TL | Code
  deriving stock (Bounded , Enum , Eq , Read , Show)
