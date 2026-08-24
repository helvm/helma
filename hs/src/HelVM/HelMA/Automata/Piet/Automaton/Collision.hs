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

-- Czysty krok przesuwania po siatce i sprawdzania odbić (bez narzutu Monady Efektów)
transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition autoMem
  | _collisionCount autoMem >= 8 = do
      logDebugN "Max collisions reached (8). Terminating."
      pure $ Trampoline.break ()
  | otherwise = case nextColour (_memory autoMem) of
      Nothing               -> transition (doIfCollided autoMem)
      Just Black          -> transition (doIfCollided autoMem)
      Just White          -> transition (stepWhite autoMem)
      Just (Chromatic c') -> stepChromatic c' autoMem

-- STEP & COLOR HANDLERS

{-# INLINE stepWhite #-}
stepWhite ∷ AutomatonMemory → AutomatonMemory
stepWhite autoMem = autoMem { _memory = stepWhitePixel (_memory autoMem) }

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m (Either () AutomatonMemory)
stepChromatic c' autoMem = do
  let oldMem = _memory autoMem
  let newMem = advancePosition oldMem
  nextMem <- stepMemory c' oldMem newMem
  pure $ Trampoline.continue $ resetCollision autoMem { _memory = nextMem }

-- COLLISION STATE MANAGEMENT

{-# INLINE doIfCollided #-}
doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = 
  let cc   = _collisionCount autoMem
      mem' = handleCollision (even cc) (_memory autoMem)
  in autoMem { _memory = mem', _collisionCount = cc + 1 }

{-# INLINE resetCollision #-}
resetCollision ∷ AutomatonMemory → AutomatonMemory
resetCollision autoMem = autoMem { _collisionCount = 0 }
