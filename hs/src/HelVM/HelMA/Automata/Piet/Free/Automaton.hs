module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( collisionCount
  , interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline                 as Trampoline

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
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (Trampoline.break ())
transitionStep _ st = Trampoline.continue <$> handleNextColour colour st pos (move dp p) block where
    mem    = st ^. memory
    prog   = programMemory mem
    dp     = directionPointerMemory mem
    pos    = positionMemory mem
    m      = prog ^. image
    block  = discoverBlock m pos
    p      = selectCodel block mem
    colour = colourAt prog (move dp p)

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → Coordinates → Coordinates → Block → m AutomatonMemory
handleNextColour Nothing st _ _ _              = pure $ doIfCollided st
handleNextColour (Just Black) st _ _ _         = pure $ doIfCollided st
handleNextColour (Just White) st _ newPos _    = pure $ setPositionState newPos 0 st
handleNextColour (Just c') st pos newPos block = evalTransitionBlock (colourAt (programMemory (st ^. memory)) pos) (setPositionState newPos 0 st) pos c' block

setPositionState ∷ Coordinates → Int → AutomatonMemory → AutomatonMemory
setPositionState pos cc st = st { _collisionCount = cc } & memory %~ setPosition pos

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → Coordinates → Color → Block → m AutomatonMemory
evalTransitionBlock (Just (Chromatic c)) st _ (Chromatic c') block = do
  let blockSize = blockCodelCount block (st ^. memory)
  mem' <- colors2Command c c' blockSize (st ^. memory)
  pure $ st & memory .~ mem'
evalTransitionBlock _ st _ _ _ = pure st

-- Collision state management

doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided st = updateCollisionCount $ st & memory %~ handleCollision (even (_collisionCount st))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount st = st { _collisionCount = _collisionCount st + 1 }
