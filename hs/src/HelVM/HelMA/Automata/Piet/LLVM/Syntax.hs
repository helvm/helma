module HelVM.HelMA.Automata.Piet.LLVM.Syntax
  ( Block (..)
  , CodelChooser (..)
  , Command (..)
  , DPCC (..)
  , DirectionPointer (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  , commandFromTransition
  , showCommand
  , showDPCC
  ) where

import           Data.Vector                          ( Vector )
import qualified Data.Vector.Generic                  as V
import           HelVM.HelMA.Automata.Piet.LLVM.Codel

newtype Block
  = Block { nextBlockTable :: Map DPCC NextBlock }
  deriving stock (Eq, Show)

data NextBlock
  = NextBlock
      { _command    :: Command
      , _dpcc       :: DPCC
      , _blockIndex :: Int
      }
  | ExitProgram
  deriving stock (Eq, Show)

data SyntaxGraph
  = SyntaxGraph
      { _initialBlockIndex :: Int
      , _initialDPCC       :: DPCC
      , _blockMap          :: IntMap Block
      }
  | EmptySyntaxGraph
  deriving stock (Eq, Show)

data DirectionPointer
  = DPRight
  | DPDown
  | DPLeft
  | DPUp
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data CodelChooser
  = CCLeft
  | CCRight
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DPCC
  = DPCC
      { getDP :: DirectionPointer
      , getCC :: CodelChooser
      }
  deriving stock (Eq, Ord, Show)

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

commandFromTransition ∷ (Hue, Lightness) → (Hue, Lightness) → Int → Command
commandFromTransition (currentHue, currentLightness) (nextHue, nextLightness) = cmd where
  cmd = commandConstructors V.! (hueDiff * 3 + lightnessDiff)
  hueDiff = (fromEnum nextHue - fromEnum currentHue) `mod` 6
  lightnessDiff = (fromEnum nextLightness - fromEnum currentLightness) `mod` 3

commandConstructors ∷ Vector (Int → Command)
commandConstructors = V.fromList [ const NoOperation
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

showDPCC ∷ DPCC → String
showDPCC (DPCC dp cc) = [charDP dp, charCC cc] where
  charDP DPRight = 'r'
  charDP DPDown  = 'd'
  charDP DPLeft  = 'l'
  charDP DPUp    = 'u'
  charCC CCLeft  = 'l'
  charCC CCRight = 'r'
