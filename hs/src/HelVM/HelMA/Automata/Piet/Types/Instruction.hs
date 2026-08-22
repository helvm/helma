module HelVM.HelMA.Automata.Piet.Types.Instruction
  ( Instruction (..)
  , step
  ) where

step ∷ Maybe Int → Maybe Int → Int → Instruction
step (Just 0) (Just 0) _ = Nop
step (Just 0) (Just 1) _ = Add
step (Just 0) (Just 2) _ = Divide
step (Just 0) (Just 3) _ = Greater
step (Just 0) (Just 4) _ = Duplicate
step (Just 0) (Just 5) _ = InChar
step (Just 1) (Just 0) n = Push n
step (Just 1) (Just 1) _ = Subtract
step (Just 1) (Just 2) _ = Mod
step (Just 1) (Just 3) _ = Pointer
step (Just 1) (Just 4) _ = Roll
step (Just 1) (Just 5) _ = OutNum
step (Just 2) (Just 0) _ = Pop
step (Just 2) (Just 1) _ = Multiply
step (Just 2) (Just 2) _ = Not
step (Just 2) (Just 3) _ = Switch
step (Just 2) (Just 4) _ = InNum
step (Just 2) (Just 5) _ = OutChar
step _        _        _ = Nop

data Instruction
  = Push Int
  | Pop
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Not
  | Greater
  | Pointer
  | Switch
  | Duplicate
  | Roll
  | InNum
  | InChar
  | OutNum
  | OutChar
  | Nop
  deriving stock (Eq, Show)
