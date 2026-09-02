module HelVM.HelMA.Automata.Piet.Types.SyntaxGraph
  ( Block (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

data SyntaxGraph
  = SyntaxGraph
      { entryBlockIndex :: Int
      , entryCourse     :: Course
      , blockMap        :: IntMap Block
      }
  deriving stock (Eq, Show)

newtype Block
  = Block { transitions :: Map Course (Maybe NextBlock) }
  deriving stock (Eq, Show)

data NextBlock
  = NextBlock
      { command    :: Command
      , course     :: Course
      , blockIndex :: Int
      }
  deriving stock (Eq, Show)
