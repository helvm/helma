module HelVM.HelMA.Automata.Piet.Types.SyntaxGraph
  ( Block (..)
  , BlockEdge (..)
  , NextBlock (..)
  , SyntaxGraph (..)
  , blockIndexL
  , blockMapL
  , commandL
  , courseL
  , entryL
  , targetL
  , transitionsL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

import           Relude.Extra

-- TYPES

data SyntaxGraph
  = SyntaxGraph
      { entry    :: !BlockEdge
      , blockMap :: !(IntMap Block)
      }
  deriving stock (Eq, Show)

newtype Block
  = Block { transitions :: Map Course (Maybe NextBlock) }
  deriving stock (Eq, Show)

data NextBlock
  = NextBlock
      { command :: !Command
      , target  :: !BlockEdge
      }
  deriving stock (Eq, Show)

data BlockEdge
  = BlockEdge
      { blockIndex :: !Int
      , course     :: !Course
      }
  deriving stock (Eq, Show)


-- LENSES: SyntaxGraph

entryL ∷ Lens' SyntaxGraph BlockEdge
entryL = lens entry $ \s x → s { entry = x }

blockMapL ∷ Lens' SyntaxGraph (IntMap Block)
blockMapL = lens blockMap $ \s x → s { blockMap = x }

-- LENSES: Block

transitionsL ∷ Lens' Block (Map Course (Maybe NextBlock))
transitionsL = lens transitions $ \_ x → Block x

-- LENSES: NextBlock

commandL ∷ Lens' NextBlock Command
commandL = lens command $ \s x → s { command = x }

targetL ∷ Lens' NextBlock BlockEdge
targetL = lens target $ \s x → s { target = x }

-- LENSES: BlockEdge

blockIndexL ∷ Lens' BlockEdge Int
blockIndexL = lens blockIndex $ \s x → s { blockIndex = x }

courseL ∷ Lens' BlockEdge Course
courseL = lens course $ \s x → s { course = x }

