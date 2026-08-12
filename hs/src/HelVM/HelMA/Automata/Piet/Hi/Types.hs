{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module HelVM.HelMA.Automata.Piet.Hi.Types
  ( Block
  , CodelChooser (..)
  , CodelSize
  , Colour (..)
  , ColourMap (..)
  , DirectionPointer (..)
  , Hue (..)
  , Instruction (..)
  , Piet
  , Position
  , Program
  , ProgramConfig (..)
  , ProgramError (..)
  , ProgramState (..)
  , add
  , codelChooser
  , collisionCount
  , currentPosition
  , directionPointer
  , divide
  , duplicate
  , greater
  , inChar
  , inNum
  , initialState
  , mapHeight
  , mapWidth
  , matrix
  , mod'
  , multiply
  , nop
  , not'
  , outChar
  , outNum
  , pointer
  , pop
  , push
  , roll
  , runPiet
  , stack
  , subtract'
  , switch
  ) where

import           Control.Monad.Free
import           Lens.Micro.TH

import qualified Data.Vector        as V

import qualified Text.Show

data DirectionPointer
  = DLeft
  | DRight
  | DUp
  | DDown
  deriving stock (Eq, Show)

data CodelChooser
  = CLeft
  | CRight
  deriving stock (Eq, Show)

data Colour
  = Light Hue
  | Normal Hue
  | Dark Hue
  | Black
  | White
  deriving stock (Eq, Show)

data Hue
  = Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving stock (Enum, Eq, Show)

data ColourMap
  = ColourMap
      { _matrix    :: V.Vector (V.Vector Colour)
      , _mapWidth  :: Int
      , _mapHeight :: Int
      }
  deriving stock (Eq, Show)

type CodelSize = Int
type Position = (Int, Int) -- (X, Y)
type Block = [Position]

data ProgramState
  = ProgramState
      { _directionPointer :: DirectionPointer
      , _codelChooser     :: CodelChooser
      , _currentPosition  :: Position
      , _stack            :: [Int]
      , _collisionCount   :: Int
      }
  deriving stock (Eq, Show)

data ProgramConfig
  = ProgramConfig
      { codelSize :: CodelSize
      , colourMap :: ColourMap
      }
  deriving stock (Eq, Show)

makeLenses ''ColourMap
makeLenses ''ProgramState
makeLenses ''ProgramConfig

data ProgramError
  = ParseInt String
  | LoadFile String
  | FindFile String
  | NotImplemented String

data Instruction r
  = Push Int r
  | Pop r
  | Add r
  | Subtract r
  | Multiply r
  | Divide r
  | Mod r
  | Not r
  | Greater r
  | Pointer r
  | Switch r
  | Duplicate r
  | Roll r
  | InNum r
  | InChar r
  | OutNum r
  | OutChar r
  | Nop r
  deriving stock (Functor)

type Program = Free Instruction ()

type Piet = ReaderT ProgramConfig (StateT ProgramState IO)

initialState ∷ ProgramState
initialState = ProgramState {
    _directionPointer = DRight,
    _codelChooser = CLeft,
    _currentPosition = (0, 0),
    _stack = [],
    _collisionCount = 0
  }

runPiet ∷ ProgramConfig → ProgramState → Piet a → IO (a, ProgramState)
runPiet conf s c = runStateT (runReaderT c conf) s

push ∷ MonadFree Instruction m ⇒ Int → m ()
push n = liftF (Push n ())

pop ∷ MonadFree Instruction m ⇒ m ()
pop = liftF (Pop ())

add ∷ MonadFree Instruction m ⇒ m ()
add = liftF (Add ())

subtract' ∷ MonadFree Instruction m ⇒ m ()
subtract' = liftF (Subtract ())

multiply ∷ MonadFree Instruction m ⇒ m ()
multiply = liftF (Multiply ())

divide ∷ MonadFree Instruction m ⇒ m ()
divide = liftF (Divide ())

mod' ∷ MonadFree Instruction m ⇒ m ()
mod' = liftF (Mod ())

not' ∷ MonadFree Instruction m ⇒ m ()
not' = liftF (Not ())

greater ∷ MonadFree Instruction m ⇒ m ()
greater = liftF (Greater ())

pointer ∷ MonadFree Instruction m ⇒ m ()
pointer = liftF (Pointer ())

switch ∷ MonadFree Instruction m ⇒ m ()
switch = liftF (Switch ())

duplicate ∷ MonadFree Instruction m ⇒ m ()
duplicate = liftF (Duplicate ())

roll ∷ MonadFree Instruction m ⇒ m ()
roll = liftF (Roll ())

inNum ∷ MonadFree Instruction m ⇒ m ()
inNum = liftF (InNum ())

inChar ∷ MonadFree Instruction m ⇒ m ()
inChar = liftF (InChar ())

outNum ∷ MonadFree Instruction m ⇒ m ()
outNum = liftF (OutNum ())

outChar ∷ MonadFree Instruction m ⇒ m ()
outChar = liftF (OutChar ())

nop ∷ MonadFree Instruction m ⇒ m ()
nop = liftF (Nop ())

instance Show ProgramError where
  show (ParseInt m)       = "Error while parsing: " <> m
  show (LoadFile m)       = "Error while loading file: " <> m
  show (FindFile m)       = "Can't find file: " <> m
  show (NotImplemented m) = m <> " hasn't been implemented yet."
