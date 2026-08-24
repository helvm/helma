module HelVM.HelMA.Automata.Piet.Free.Automaton
  ( collisionCount
  , interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline               as Trampoline

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
transitionStep _ autoMem = Trampoline.continue <$> handleNextColour (nextColour mem) autoMem where
  mem  = autoMem ^. memory

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m AutomatonMemory
handleNextColour Nothing               = pure . doIfCollided
handleNextColour (Just Black)          = pure . doIfCollided
handleNextColour (Just White)          = pure . stepWhite
handleNextColour (Just (Chromatic c')) = stepChromatic c'

stepWhite ∷ AutomatonMemory → AutomatonMemory
stepWhite autoMem = setPositionState (nextCodelPos mem) autoMem where mem = autoMem ^. memory

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m AutomatonMemory
stepChromatic c' autoMem = evalTransitionBlock (currentColour mem) c' mem (setPositionState newPos autoMem) where
  newPos = nextCodelPos mem
  mem    = autoMem ^. memory

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Memory → AutomatonMemory → m AutomatonMemory
evalTransitionBlock c c' mem autoMem = flip (set memory) autoMem <$> evalTransitionBlockMemory c c' mem (autoMem ^. memory)

setPositionState ∷ Coordinates → AutomatonMemory → AutomatonMemory
setPositionState pos autoMem = autoMem { _collisionCount = 0 } & memory %~ setPosition pos

-- Collision state management

doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = updateCollisionCount $ autoMem & memory %~ handleCollision (even (_collisionCount autoMem))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount autoMem = autoMem { _collisionCount = _collisionCount autoMem + 1 }
