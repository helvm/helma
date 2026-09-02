module HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
  ( Block (..)
  , NextBlock (..)
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
  = NextBlockJust NextBlock
  | ExitProgram
  deriving stock (Eq, Show)

data NextBlock
  = NextBlock
      { _command    :: Command
      , _course     :: Course
      , _blockIndex :: Int
      }
  deriving stock (Eq, Show)
