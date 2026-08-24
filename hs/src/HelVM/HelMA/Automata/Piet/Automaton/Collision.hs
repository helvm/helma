module HelVM.HelMA.Automata.Piet.Automaton.Collision
  ( collisionCount
  , memory
  , start
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline               as Trampoline

import           Control.Monad.Logger

import           Lens.Micro.Platform

-- TYPES & LENSES

data AutomatonMemory
  = AutomatonMemory
      { _memory         :: !Memory
      , _collisionCount :: {-# UNPACK #-} !Int
      }

makeLenses ''AutomatonMemory

-- TOP-LEVEL DRIVER

initialState ∷ Program → AutomatonMemory
initialState p = AutomatonMemory
  { _memory         = initialMemory p
  , _collisionCount = 0
  }

start ∷ AppSafeEff m ⇒ Program → m ()
start = Trampoline.trampolineM transition . initialState

transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition autoMem
  | autoMem ^. collisionCount >= 8 = Trampoline.break () <$ logDebugN "Max collisions reached (8). Terminating."
  | otherwise                      = stepByColour (nextColour (autoMem ^. memory)) autoMem

-- STEP & COLOR HANDLERS

stepByColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m (Either () AutomatonMemory)
stepByColour Nothing               autoMem = transition (doIfCollided autoMem)
stepByColour (Just Black)          autoMem = transition (doIfCollided autoMem)
stepByColour (Just White)          autoMem = transition (stepWhite autoMem)
stepByColour (Just (Chromatic c')) autoMem = stepChromatic c' autoMem

{-# INLINE stepWhite #-}
stepWhite ∷ AutomatonMemory → AutomatonMemory
stepWhite autoMem = autoMem & memory %~ stepWhitePixel

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m (Either () AutomatonMemory)
stepChromatic c' autoMem = makeNext <$> stepMemory c' oldMem (advancePosition oldMem) where
  makeNext nextMem = Trampoline.continue $ resetCollision autoMem { _memory = nextMem }
  oldMem           = autoMem ^. memory

-- COLLISION STATE MANAGEMENT

{-# INLINE doIfCollided #-}
doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = updateCollisionCount $ autoMem & memory %~ handleCollision (even (autoMem ^. collisionCount))

{-# INLINE updateCollisionCount #-}
updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount autoMem = autoMem { _collisionCount = autoMem ^. collisionCount + 1 }

{-# INLINE resetCollision #-}
resetCollision ∷ AutomatonMemory → AutomatonMemory
resetCollision autoMem = autoMem { _collisionCount = 0 }
