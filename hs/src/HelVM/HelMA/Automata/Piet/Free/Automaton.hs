module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Free.InstructionFF

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Instruction
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter     ( position )
import qualified HelVM.HelMA.Automata.Piet.Types.InstructionCounter     as IC
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory      hiding ( program )
import qualified HelVM.HelMA.Automata.Piet.Types.InstructionMemory      as IM
import qualified HelVM.HelMA.Automata.Piet.Types.Orientation            as Orientation
import           HelVM.HelMA.Automata.Piet.Types.Program
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
interpret ∷ AppSafeEff m ⇒ Program → m ()
interpret p = loop $ initialState p where
  loop st = transition st >>= \case
    Right st' -> loop st'
    Left ()   -> pass

transition ∷ AppSafeEff m ⇒ ProgramState → m (Either () ProgramState)
transition st = transitionStep (_collisionCount st) st

transitionStep ∷ AppSafeEff m ⇒ Int → ProgramState → m (Either () ProgramState)
transitionStep cc _
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (Left ())
transitionStep _ st =
  handleNextColour colour st pos (move dp p) block
  where
    prog   = st ^. im . IM.program
    dp     = directionPointerIM (st ^. im)
    pos    = st ^. im . instructionCounter . position
    m      = prog ^. image
    block  = discoverBlock m pos
    p      = selectCodel st block
    colour = colourAt prog (move dp p)

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → ProgramState → Coordinates → Coordinates → Block → m (Either () ProgramState)
handleNextColour Nothing st _ _ _           = pure $ Right (doIfCollided st)
handleNextColour (Just Black) st _ _ _     = pure $ Right (doIfCollided st)
handleNextColour (Just White) st _ newPos _ = pure $ Right (setPosition newPos 0 st)
handleNextColour (Just c') st pos newPos block =
  Right <$> evalTransitionBlock (colourAt (st ^. im . IM.program) pos) (setPosition newPos 0 st) pos c' block

setPosition ∷ Coordinates → Int → ProgramState → ProgramState
setPosition pos cc st = st { _collisionCount = cc } & im . instructionCounter . position .~ pos

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → ProgramState → Coordinates → Color → Block → m ProgramState
evalTransitionBlock (Just c) st _ c' block
  | c /= White = interpretF (colorsToProgram c c' (blockCodelCount (st ^. im . IM.program . codelSize) block)) st
evalTransitionBlock _ st _ _ _ = pure st

blockCodelCount ∷ CodelSize → Block → Int
blockCodelCount cs block = olength block `div` (cs * cs)

-- Board and Color queries
discoverBlock ∷ Image Color → Coordinates → Block
discoverBlock m startPos = S.toList $ go S.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `S.member` visited  = visited
    | m &! pos /= targetColor = visited
    | otherwise               = L.foldl' go (S.insert pos visited) (neighbours pos)

selectCodel ∷ ProgramState → Block → Coordinates
selectCodel st = L.maximumBy (Orientation.furthest (st ^. im . instructionCounter . IC.orientation))

colourAt ∷ Program → Coordinates → Maybe Color
colourAt prog pos = (prog ^. image) &! pos

infixl 9 &!
(&!) ∷ Image Color → Coordinates → Maybe Color
m &! coord
  | inRangeImage coord m = Just $ pixelImage coord m
  | otherwise            = Nothing

-- Collision state management
doIfCollided ∷ ProgramState → ProgramState
doIfCollided st = updateCollisionCount (handleCollision (even (_collisionCount st)) st)

updateCollisionCount ∷ ProgramState → ProgramState
updateCollisionCount st = st { _collisionCount = _collisionCount st + 1 }

handleCollision ∷ Bool → ProgramState → ProgramState
handleCollision True  = toggleChooser
handleCollision False = rotatePointer

toggleChooser ∷ ProgramState → ProgramState
toggleChooser = im %~ toggleCodelChooserIM 1

rotatePointer ∷ ProgramState → ProgramState
rotatePointer = im %~ rotateDirectionPointerIM 1

-- Instruction generation
colorsToProgram ∷ Color → Color → Int → InstructionFF
colorsToProgram c c' n = liftF $ InstructionF (colorsToInstruction c c' n) ()

colorsToInstruction ∷ Color → Color → Int → Instruction
colorsToInstruction c c' = step (lightnessSteps c c') (hueSteps c c')

-- AST Interpreter
interpretF ∷ AppSafeEff m ⇒ InstructionFF → ProgramState → m ProgramState
interpretF (Pure _) st                  = pure st
interpretF (Free (InstructionF i r)) st = evalInstruction i r st

evalInstruction ∷ AppSafeEff m ⇒ Instruction → InstructionFF → ProgramState → m ProgramState
evalInstruction (Push n)  r st = evalStack ("push " <> show n) (pure . ALU.push1 n) r st
evalInstruction Pop       r st = evalStack "pop" ALU.discard r st
evalInstruction Add       r st = evalStack "add" (ALU.binaryInstruction ST.Add) r st
evalInstruction Subtract  r st = evalStack "subtract" (ALU.binaryInstruction ST.Sub) r st
evalInstruction Multiply  r st = evalStack "multiply" (ALU.binaryInstruction ST.Mul) r st
evalInstruction Divide    r st = evalStack "divide" (ALU.binaryInstruction ST.Div) r st
evalInstruction Mod       r st = evalStack "mod" (ALU.binaryInstruction ST.Mod) r st
evalInstruction Not       r st = evalStack "not" ALU.lNot r st
evalInstruction Greater   r st = evalStack "greater" (ALU.binaryInstruction ST.LGT) r st
evalInstruction Pointer   r st = evalFlip "pointer" rotateDirectionPointerIM r st
evalInstruction Switch    r st = evalFlip "switch" toggleCodelChooserIM r st
evalInstruction Duplicate r st = evalStack "duplicate" (ALU.copy 0) r st
evalInstruction Roll      r st = evalStack "roll" ALU.roll r st
evalInstruction InNum     r st = evalStack "in_number" ALU.inputDec r st
evalInstruction InChar    r st = evalStack "in_char" ALU.inputChar r st
evalInstruction OutNum    r st = evalStack "out_number" ALU.outputDecMaybe r st
evalInstruction OutChar   r st = evalStack "out_char" ALU.outputCharMaybe r st
evalInstruction Nop       r st = interpretF r st

evalStack ∷ AppSafeEff m ⇒ Text → ([Int] → m [Int]) → InstructionFF → ProgramState → m ProgramState
evalStack name f r st = logMsg st name *> (setStack st <$> f (_stack st)) >>= interpretF r

setStack ∷ ProgramState → [Int] → ProgramState
setStack st s = st { _stack = s }

evalFlip ∷ AppSafeEff m ⇒ Text → (Int → InstructionMemory → InstructionMemory) → InstructionFF → ProgramState → m ProgramState
evalFlip _ _ r st@ProgramState{ _stack = [] } = interpretF r st
evalFlip name f r st@ProgramState{ _stack = x:_ } = do
  let st' = st & im %~ f x
  logMsg st' (name <> " " <> show (directionPointerIM (st' ^. im)))
  interpretF r st'

logMsg ∷ AppSafeEff m ⇒ ProgramState → Text → m ()
logMsg st msg = logWithPosition msg (st ^. im)
