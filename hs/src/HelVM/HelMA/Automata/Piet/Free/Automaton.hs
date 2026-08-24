module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( collisionCount
  , interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
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
interpret  = Trampoline.trampolineM transition . initialState

transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition autoMem = transitionStep (_collisionCount autoMem) autoMem

transitionStep ∷ AppSafeEff m ⇒ Int → AutomatonMemory → m (Either () AutomatonMemory)
transitionStep cc _
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (Trampoline.break ())
transitionStep _ autoMem = Trampoline.continue <$> handleNextColour (colourAt prog (nextCodelPos mem)) autoMem where
  prog = programMemory mem
  mem  = autoMem ^. memory

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m AutomatonMemory
handleNextColour Nothing          autoMem      = pure $ doIfCollided autoMem
handleNextColour (Just Black)     autoMem      = pure $ doIfCollided autoMem
handleNextColour (Just White)     autoMem      = pure $ setPositionState (nextCodelPos mem) 0 autoMem where mem = autoMem ^. memory
handleNextColour (Just (Chromatic c')) autoMem = stepChromatic c' autoMem

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m AutomatonMemory
stepChromatic c' autoMem = evalTransitionBlock (colourAt (programMemory mem) pos) c' block (setPositionState newPos 0 autoMem) where
  newPos  = move (directionPointerMemory mem) (selectCodel block mem)
  block   = discoverBlock (programMemory mem ^. image) pos
  pos     = positionMemory mem
  mem     = autoMem ^. memory

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Block → AutomatonMemory → m AutomatonMemory
evalTransitionBlock (Just (Chromatic c)) c' block autoMem = do
  let blockSize = blockCodelCount block (autoMem ^. memory)
  mem' <- colors2Command c c' blockSize (autoMem ^. memory)
  pure $ autoMem & memory .~ mem'
evalTransitionBlock _ _ _ autoMem = pure autoMem

setPositionState ∷ Coordinates → Int → AutomatonMemory → AutomatonMemory
setPositionState pos cc autoMem = autoMem { _collisionCount = cc } & memory %~ setPosition pos

-- Collision state management

doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = updateCollisionCount $ autoMem & memory %~ handleCollision (even (_collisionCount autoMem))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount autoMem = autoMem { _collisionCount = _collisionCount autoMem + 1 }
