module HelVM.HelMA.Automata.Piet.Types.SyntaxGraph
  ( Block (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  , blockMapL
  , entryBlockIndexL
  , entryCourseL
  , transitionsL
  , blockIndexL
  , commandL
  , courseL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

import           Relude.Extra

-- TYPES

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

-- LENSES: SyntaxGraph

entryBlockIndexL ∷ Lens' SyntaxGraph Int
entryBlockIndexL = lens entryBlockIndex $ \s x → s { entryBlockIndex = x }

entryCourseL ∷ Lens' SyntaxGraph Course
entryCourseL = lens entryCourse $ \s x → s { entryCourse = x }

blockMapL ∷ Lens' SyntaxGraph (IntMap Block)
blockMapL = lens blockMap $ \s x → s { blockMap = x }

-- LENSES: Block

transitionsL ∷ Lens' Block (Map Course (Maybe NextBlock))
transitionsL = lens transitions $ \_ x → Block x

-- LENSES: NextBlock

commandL ∷ Lens' NextBlock Command
commandL = lens command $ \s x → s { command = x }

courseL ∷ Lens' NextBlock Course
courseL = lens course $ \s x → s { course = x }

blockIndexL ∷ Lens' NextBlock Int
blockIndexL = lens blockIndex $ \s x → s { blockIndex = x }
