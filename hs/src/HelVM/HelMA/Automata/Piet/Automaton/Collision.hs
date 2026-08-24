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
      , _collisionCount :: !Int
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
  | autoMem ^. collisionCount >= 8 = do
      logDebugN "Max collisions reached (8). Terminating."
      pure $ Trampoline.break ()
  | otherwise = Trampoline.continue <$> handleNextColour (nextColour mem) autoMem
  where
    mem = autoMem ^. memory

-- STEP & COLOR HANDLERS

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m AutomatonMemory
handleNextColour Nothing               = pure . doIfCollided
handleNextColour (Just Black)          = pure . doIfCollided
handleNextColour (Just White)          = pure . stepWhite
handleNextColour (Just (Chromatic c')) = stepChromatic c'

stepWhite ∷ AutomatonMemory → AutomatonMemory
stepWhite autoMem = updateMemory autoMem $ advancePosition $ autoMem ^. memory

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m AutomatonMemory
stepChromatic c' autoMem = updateMemory autoMem <$> stepMemory c' oldMem newMem where
  newMem = advancePosition oldMem
  oldMem = autoMem ^. memory

-- COLLISION STATE MANAGEMENT

doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided autoMem = updateCollisionCount $ autoMem & memory %~ handleCollision (even (_collisionCount autoMem))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount autoMem = autoMem { _collisionCount = _collisionCount autoMem + 1 }

updateMemory ∷ AutomatonMemory → Memory → AutomatonMemory
updateMemory autoMem mem = resetCollision autoMem & memory .~ mem

resetCollision ∷ AutomatonMemory → AutomatonMemory
resetCollision autoMem = autoMem { _collisionCount = 0 }
