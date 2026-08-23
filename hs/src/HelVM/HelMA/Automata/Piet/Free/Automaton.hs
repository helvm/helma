module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( collisionCount
  , interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Free.InstructionFF

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Instruction
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program

import qualified HelVM.HelMA.Automaton.Combiner.ALU                     as ALU
import           HelVM.HelMA.Automaton.Eff.MonadEff
import qualified HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction as ST
import           HelVM.HelMA.Automaton.Trampoline                       as Trampoline

import           Control.Monad.Free
import           Control.Monad.Logger

import           Lens.Micro.Platform

-- Top-level driver

initialState ∷ Program → AutomatonMemory
initialState p = AutomatonMemory
  { _memory         = initialMemory p
  , _collisionCount = 0
  }

data AutomatonMemory
  = AutomatonMemory
      { _memory         :: !Memory
      , _collisionCount :: !Int
      }

makeLenses ''AutomatonMemory

interpret ∷ AppSafeEff m ⇒ Program → m ()
interpret p = Trampoline.trampolineM transition $ initialState p

transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition st = transitionStep (_collisionCount st) st

transitionStep ∷ AppSafeEff m ⇒ Int → AutomatonMemory → m (Either () AutomatonMemory)
transitionStep cc _
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (Left ())
transitionStep _ st =
  handleNextColour colour st pos (move dp p) block
  where
    mem    = st ^. memory
    prog   = programMemory mem
    dp     = directionPointerMemory mem
    pos    = positionMemory mem
    m      = prog ^. image
    block  = discoverBlock m pos
    p      = selectCodel block mem
    colour = colourAt prog (move dp p)

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → Coordinates → Coordinates → Block → m (Either () AutomatonMemory)
handleNextColour Nothing st _ _ _           = pure $ Right (doIfCollided st)
handleNextColour (Just Black) st _ _ _     = pure $ Right (doIfCollided st)
handleNextColour (Just White) st _ newPos _ = pure $ Right (setPositionState newPos 0 st)
handleNextColour (Just c') st pos newPos block =
  Right <$> evalTransitionBlock (colourAt (programMemory (st ^. memory)) pos) (setPositionState newPos 0 st) pos c' block

setPositionState ∷ Coordinates → Int → AutomatonMemory → AutomatonMemory
setPositionState pos cc st = st { _collisionCount = cc } & memory %~ setPosition pos

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → Coordinates → Color → Block → m AutomatonMemory
evalTransitionBlock (Just c) st _ c' block
  | c /= White = interpretF (colorsToProgram c c' (blockCodelCount block (st ^. memory))) st
evalTransitionBlock _ st _ _ _ = pure st

-- Collision state management
doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided st = updateCollisionCount $ st & memory %~ handleCollision (even (_collisionCount st))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount st = st { _collisionCount = _collisionCount st + 1 }

-- Instruction generation
colorsToProgram ∷ Color → Color → Int → InstructionFF
colorsToProgram c c' n = liftF $ InstructionF (colorsToInstruction c c' n) ()

colorsToInstruction ∷ Color → Color → Int → Instruction
colorsToInstruction c c' = step (lightnessSteps c c') (hueSteps c c')

-- AST Interpreter
interpretF ∷ AppSafeEff m ⇒ InstructionFF → AutomatonMemory → m AutomatonMemory
interpretF (Pure _) st                  = pure st
interpretF (Free (InstructionF i r)) st = evalInstruction i r st

evalInstruction ∷ AppSafeEff m ⇒ Instruction → InstructionFF → AutomatonMemory → m AutomatonMemory
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

evalStack ∷ AppSafeEff m ⇒ Text → (Stack → m Stack) → InstructionFF → AutomatonMemory → m AutomatonMemory
evalStack name f r st = do
  mem' <- modifyStackWithLog name f (st ^. memory)
  interpretF r (st & memory .~ mem')

evalFlip ∷ AppSafeEff m ⇒ Text → (Int → InstructionMemory → InstructionMemory) → InstructionFF → AutomatonMemory → m AutomatonMemory
evalFlip name f r st = modifyFlipWithLog name f (st ^. memory) >>= \case
  Nothing   -> interpretF r st
  Just mem' -> interpretF r (st & memory .~ mem')
