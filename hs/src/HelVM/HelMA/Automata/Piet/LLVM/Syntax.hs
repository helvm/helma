module HelVM.HelMA.Automata.Piet.LLVM.Syntax
  ( Block (..)
  , CodelChooser (..)
  , Command (..)
  , Course (..)
  , DirectionPointer (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  , commandFromTransition
  , showCommand
  , showCourse
  ) where


import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

newtype Block
  = Block { nextBlockTable :: Map Course NextBlock }
  deriving stock (Eq, Show)

data NextBlock
  = NextBlock
      { _command    :: Command
      , _course     :: Course
      , _blockIndex :: Int
      }
  | ExitProgram
  deriving stock (Eq, Show)

data SyntaxGraph
  = SyntaxGraph
      { _initialBlockIndex :: Int
      , _initialCourse     :: Course
      , _blockMap          :: IntMap Block
      }
  | EmptySyntaxGraph
  deriving stock (Eq, Show)
