module HelVM.HelMA.Automata.Piet.Automaton.Collision
  ( collisionCountL
  , memoryL
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

import           Relude.Extra

-- TYPES & LENSES

data AutomatonMemory
  = AutomatonMemory
      { memory         :: !Memory
      , collisionCount :: {-# UNPACK #-} !Int
      }

memoryL ∷ Lens' AutomatonMemory Memory
memoryL = lens memory (\s x -> s { memory = x })

collisionCountL ∷ Lens' AutomatonMemory Int
collisionCountL = lens collisionCount (\s x -> s { collisionCount = x })

-- TOP-LEVEL DRIVER

initialState ∷ Program → AutomatonMemory
initialState p = AutomatonMemory
  { memory         = initialMemory p
  , collisionCount = 0
  }

start ∷ AppSafeEff m ⇒ Program → m ()
start = Trampoline.trampolineM transition . initialState

transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition autoMem
  | autoMem ^. collisionCountL >= 8 = Trampoline.break () <$ logDebugN "Max collisions reached (8). Terminating."
  | otherwise                       = stepByColour (nextColour (autoMem ^. memoryL)) autoMem

-- STEP & COLOR HANDLERS

stepByColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m (Either () AutomatonMemory)
stepByColour Nothing               autoMem = transition (doIfCollided autoMem)
stepByColour (Just Black)          autoMem = transition (doIfCollided autoMem)
stepByColour (Just White)          autoMem = transition (stepWhite autoMem)
stepByColour (Just (Chromatic c')) autoMem = stepChromatic c' autoMem

{-# INLINE stepWhite #-}
stepWhite ∷ AutomatonMemory → AutomatonMemory
stepWhite autoMem = autoMem & memoryL %~ stepWhitePixel

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m (Either () AutomatonMemory)
stepChromatic c' autoMem = makeNext <$> stepMemory c' oldMem (advancePosition oldMem) where
  makeNext nextMem = Trampoline.continue $ resetCollision autoMem { memory = nextMem }
  oldMem           = autoMem ^. memoryL

-- COLLISION STATE MANAGEMENT

{-# INLINE doIfCollided #-}
doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = updateCollisionCount $ autoMem & memoryL %~ handleCollision (even (autoMem ^. collisionCountL))

{-# INLINE updateCollisionCount #-}
updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount autoMem = autoMem { collisionCount = autoMem ^. collisionCountL + 1 }

{-# INLINE resetCollision #-}
resetCollision ∷ AutomatonMemory → AutomatonMemory
resetCollision autoMem = autoMem { collisionCount = 0 }
