module HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
  ( Block (..)
  , NextBlock (..)
  , NextBlockMaybe
  , SyntaxGraph (..)
  , SyntaxGraphMaybe (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

data SyntaxGraphMaybe
  = SyntaxGraphJust SyntaxGraph
  | EmptySyntaxGraph
  deriving stock (Eq, Show)

data SyntaxGraph
  = SyntaxGraph
      { _initialBlockIndex :: Int
      , _initialCourse     :: Course
      , _blockMap          :: IntMap Block
      }
  deriving stock (Eq, Show)

newtype Block
  = Block { nextBlockTable :: Map Course NextBlockMaybe }
  deriving stock (Eq, Show)

type NextBlockMaybe = Maybe NextBlock

data NextBlock
  = NextBlock
      { _command    :: Command
      , _course     :: Course
      , _blockIndex :: Int
      }
  deriving stock (Eq, Show)
