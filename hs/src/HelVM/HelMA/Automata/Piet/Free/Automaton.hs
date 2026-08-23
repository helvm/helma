module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( transition
  ) where

import           HelVM.HelMA.Automata.Piet.Free.Program

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Image                  as PietImage
import           HelVM.HelMA.Automata.Piet.Types.Instruction
import qualified HelVM.HelMA.Automata.Piet.Types.Orientation            as Orientation
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig
import           HelVM.HelMA.Automata.Piet.Types.ProgramState

import qualified HelVM.HelMA.Automaton.Combiner.ALU                     as ALU
import           HelVM.HelMA.Automaton.Eff.MonadEff
import qualified HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction as ST

import           Control.Monad.Free
import           Control.Monad.Logger                                   ( logDebugN )

import qualified Data.List                                              as L
import           Data.MonoTraversable
import qualified Data.Set                                               as S
import           Lens.Micro.Platform

import           Prelude                                                hiding ( getLine )

-- Top-level driver
transition :: AppSafeEff m => ProgramConfig -> ProgramState -> m (Bool, ProgramState)
transition conf st = transitionStep (_collisionCount st) conf st

transitionStep :: AppSafeEff m => Int -> ProgramConfig -> ProgramState -> m (Bool, ProgramState)
transitionStep cc _ st
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (False, st)
transitionStep _ conf st =
  handleNextColour colour conf st pos (move dp p) block
  where
    dp     = _directionPointer st
    pos    = _currentPosition st
    m      = conf ^. colorMap
    block  = discoverBlock m pos
    p      = selectCodel st block
    colour = colourAt conf (move dp p)

handleNextColour :: AppSafeEff m => Maybe Color -> ProgramConfig -> ProgramState -> Coordinates -> Coordinates -> Block -> m (Bool, ProgramState)
handleNextColour Nothing _ st _ _ _           = pure (True, doIfCollided st)
handleNextColour (Just Black) _ st _ _ _     = pure (True, doIfCollided st)
handleNextColour (Just White) _ st _ newPos _ = pure (True, setPosition newPos 0 st)
handleNextColour (Just c') conf st pos newPos block =
  (True ,) <$> evalTransitionBlock (colourAt conf pos) conf (setPosition newPos 0 st) pos c' block

setPosition :: Coordinates -> Int -> ProgramState -> ProgramState
setPosition pos cc st = st { _currentPosition = pos, _collisionCount = cc }

evalTransitionBlock :: AppSafeEff m => Maybe Color -> ProgramConfig -> ProgramState -> Coordinates -> Color -> Block -> m ProgramState
evalTransitionBlock (Just c) conf st _ c' block
  | c /= White = interpret (colorsToProgram c c' (blockCodelCount (conf ^. codelSize) block)) conf st
evalTransitionBlock _ _ st _ _ _ = pure st

blockCodelCount :: CodelSize -> Block -> Int
blockCodelCount cs block = olength block `div` (cs * cs)

-- Board and Color queries
discoverBlock :: PietImage.Image Color -> Coordinates -> Block
discoverBlock m startPos = S.toList $ go S.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `S.member` visited  = visited
    | m &! pos /= targetColor = visited
    | otherwise               = L.foldl' go (S.insert pos visited) (neighbours pos)

selectCodel :: ProgramState -> Block -> Coordinates
selectCodel st = L.maximumBy (Orientation.furthest (Orientation.Orientation (_directionPointer st) (_codelChooser st)))

colourAt :: ProgramConfig -> Coordinates -> Maybe Color
colourAt conf pos = (conf ^. colorMap) &! pos

infixl 9 &!
(&!) :: PietImage.Image Color -> Coordinates -> Maybe Color
m &! coord
  | PietImage.inRangeImage coord m = Just $ PietImage.pixelImage coord m
  | otherwise                      = Nothing

-- Collision state management
doIfCollided :: ProgramState -> ProgramState
doIfCollided st = updateCollisionCount (handleCollision (even (_collisionCount st)) st)

updateCollisionCount :: ProgramState -> ProgramState
updateCollisionCount st = st { _collisionCount = _collisionCount st + 1 }

handleCollision :: Bool -> ProgramState -> ProgramState
handleCollision True  = toggleChooser
handleCollision False = rotatePointer

toggleChooser :: ProgramState -> ProgramState
toggleChooser st = st { _codelChooser = nextChooser (_codelChooser st) }

rotatePointer :: ProgramState -> ProgramState
rotatePointer st = st { _directionPointer = nextPointer (_directionPointer st) }

-- Instruction generation
colorsToProgram :: Color -> Color -> Int -> Program
colorsToProgram c c' n = liftF $ InstructionF (colorsToInstruction c c' n) ()

colorsToInstruction :: Color -> Color -> Int -> Instruction
colorsToInstruction c c' = step (lightnessSteps c c') (hueSteps c c')

-- AST Interpreter
interpret :: AppSafeEff m => Program -> ProgramConfig -> ProgramState -> m ProgramState
interpret (Pure _) _ st                     = pure st
interpret (Free (InstructionF i r)) conf st = evalInstruction i r conf st

evalInstruction :: AppSafeEff m => Instruction -> Program -> ProgramConfig -> ProgramState -> m ProgramState
evalInstruction (Push n)  r conf st = evalStack ("push " <> show n) (pure . ALU.push1 n) r conf st
evalInstruction Pop       r conf st = evalStack "pop" ALU.discard r conf st
evalInstruction Add       r conf st = evalStack "add" (ALU.binaryInstruction ST.Add) r conf st
evalInstruction Subtract  r conf st = evalStack "subtract" (ALU.binaryInstruction ST.Sub) r conf st
evalInstruction Multiply  r conf st = evalStack "multiply" (ALU.binaryInstruction ST.Mul) r conf st
evalInstruction Divide    r conf st = evalStack "divide" (ALU.binaryInstruction ST.Div) r conf st
evalInstruction Mod       r conf st = evalStack "mod" (ALU.binaryInstruction ST.Mod) r conf st
evalInstruction Not       r conf st = evalStack "not" ALU.lNot r conf st
evalInstruction Greater   r conf st = evalStack "greater" (ALU.binaryInstruction ST.LGT) r conf st
evalInstruction Pointer   r conf st = evalFlip "pointer" 4 rotatePointer r conf st
evalInstruction Switch    r conf st = evalFlip "switch"  2 toggleChooser r conf st
evalInstruction Duplicate r conf st = evalStack "duplicate" (ALU.copy 0) r conf st
evalInstruction Roll      r conf st = evalStack "roll" ALU.roll r conf st
evalInstruction InNum     r conf st = evalStack "in_number" ALU.inputDec r conf st
evalInstruction InChar    r conf st = evalStack "in_char" ALU.inputChar r conf st
evalInstruction OutNum    r conf st = evalStack "out_number" ALU.outputDecMaybe r conf st
evalInstruction OutChar   r conf st = evalStack "out_char" ALU.outputCharMaybe r conf st
evalInstruction Nop       r conf st = interpret r conf st

evalStack :: AppSafeEff m => Text -> ([Int] -> m [Int]) -> Program -> ProgramConfig -> ProgramState -> m ProgramState
evalStack name f r conf st =
  logMsg st name *> (setStack st <$> f (_stack st)) >>= interpret r conf

setStack :: ProgramState -> [Int] -> ProgramState
setStack st s = st { _stack = s }

evalFlip :: AppSafeEff m => Text -> Int -> (ProgramState -> ProgramState) -> Program -> ProgramConfig -> ProgramState -> m ProgramState
evalFlip _ _ _ r conf st@ProgramState{ _stack = [] } = interpret r conf st
evalFlip name n f r conf st@ProgramState{ _stack = x:_ } = do
  let st' = applyRotations (x `mod` n) f st
  logMsg st' (name <> " " <> show (_directionPointer st'))
  interpret r conf st'

applyRotations :: Int -> (a -> a) -> a -> a
applyRotations count f st = foldr ($) st (replicate count f)

logMsg :: AppSafeEff m => ProgramState -> Text -> m ()
logMsg st msg = logDebugN $ formatLog (_currentPosition st) msg

formatLog :: Coordinates -> Text -> Text
formatLog (x, y) msg = "(" <> show x <> "," <> show y <> ") " <> msg
