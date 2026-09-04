module HelVM.HelMA.Automata.Piet.Types.Command
  ( Command (..)
  , commandFromTransition
  , renderCommand
  , showCommand
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor

import           Data.Vector                                    ( Vector )
import qualified Data.Vector.Generic                            as V

-- TYPES

data Command
  = NoOperation
  | Push Int
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
  | InNumber
  | InChar
  | OutNumber
  | OutChar
  deriving stock (Eq, Show)

-- FUNCTIONS

commandFromTransition ∷ ChromaticColor → ChromaticColor  → Int → Command
commandFromTransition c1 c2 = commandConstructorsVector V.! chromaticDiffToIndex c1 c2

commandConstructorsVector ∷ Vector (Int → Command)
commandConstructorsVector = V.fromList commandConstructors

commandConstructors ∷ [Int → Command]
commandConstructors =
  [ const NoOperation
  , Push
  , const Pop
  , const Add
  , const Subtract
  , const Multiply
  , const Divide
  , const Mod
  , const Not
  , const Greater
  , const Pointer
  , const Switch
  , const Duplicate
  , const Roll
  , const InNumber
  , const InChar
  , const OutNumber
  , const OutChar
  ]

showCommand ∷ Command → String
showCommand NoOperation = "noop"
showCommand (Push n)    = "push " ++ show n
showCommand Pop         = "pop"
showCommand Add         = "add"
showCommand Subtract    = "subtract"
showCommand Multiply    = "multiply"
showCommand Divide      = "divide"
showCommand Mod         = "mod"
showCommand Not         = "not"
showCommand Greater     = "greater"
showCommand Pointer     = "pointer"
showCommand Switch      = "switch"
showCommand Duplicate   = "duplicate"
showCommand Roll        = "roll"
showCommand InNumber    = "in (number)"
showCommand InChar      = "in (char)"
showCommand OutNumber   = "out (number)"
showCommand OutChar     = "out (char)"

renderCommand ∷ Command → Text
renderCommand NoOperation = "nop"
renderCommand (Push n)    = "push " <> show n
renderCommand Pop         = "pop"
renderCommand Add         = "add"
renderCommand Subtract    = "sub"
renderCommand Multiply    = "mul"
renderCommand Divide      = "div"
renderCommand Mod         = "mod"
renderCommand Not         = "not"
renderCommand Greater     = "greater"
renderCommand Pointer     = "pointer"
renderCommand Switch      = "switch"
renderCommand Duplicate   = "dup"
renderCommand Roll        = "roll"
renderCommand InNumber    = "in_num"
renderCommand InChar      = "in_char"
renderCommand OutNumber   = "out_num"
renderCommand OutChar     = "out_char"
