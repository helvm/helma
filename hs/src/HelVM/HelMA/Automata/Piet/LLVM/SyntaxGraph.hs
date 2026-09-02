module HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
  ( Block (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

data SyntaxGraph
  = SyntaxGraph
      { _initialBlockIndex :: Int
      , _initialCourse     :: Course
      , _blockMap          :: IntMap Block
      }
  | EmptySyntaxGraph
  deriving stock (Eq, Show)

newtype Block
  = Block { nextBlockTable :: Map Course NextBlock }
  deriving stock (Eq, Show)

data NextBlock
  = NextBlockJust
      { _command    :: Command
      , _course     :: Course
      , _blockIndex :: Int
      }
  | ExitProgram
  deriving stock (Eq, Show)
