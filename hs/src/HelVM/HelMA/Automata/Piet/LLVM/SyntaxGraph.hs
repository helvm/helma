module HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
  ( Block (..)
  , NextBlockMaybe (..)
  , SyntaxGraphMaybe (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

data SyntaxGraphMaybe
  = SyntaxGraphJust
      { _initialBlockIndex :: Int
      , _initialCourse     :: Course
      , _blockMap          :: IntMap Block
      }
  | EmptySyntaxGraph
  deriving stock (Eq, Show)

newtype Block
  = Block { nextBlockTable :: Map Course NextBlockMaybe }
  deriving stock (Eq, Show)

data NextBlockMaybe
  = NextBlockJust
      { _command    :: Command
      , _course     :: Course
      , _blockIndex :: Int
      }
  | ExitProgram
  deriving stock (Eq, Show)
