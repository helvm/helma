module HelVM.HelMA.Automata.False.Instruction where

data Instruction =
    Put Integer

  | Dup
  | Drop
  | Swap
  | Rot
  | Pick

  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | And
  | Or
  | Not

  | GT
  | EQ

  | Comment
  | Exec
  | If
  | While

  | Store
  | Fetch

  deriving stock (Eq , Show , Read)
