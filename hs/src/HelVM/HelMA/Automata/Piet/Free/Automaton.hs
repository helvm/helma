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
interpret p = Trampoline.trampolineM transition $ initialState p

transition ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
transition st = transitionStep (_collisionCount st) st

transitionStep ∷ AppSafeEff m ⇒ Int → AutomatonMemory → m (Either () AutomatonMemory)
transitionStep cc _
  | cc >= 8   = logDebugN "Max collisions reached (8). Terminating." >> pure (Trampoline.break ())
transitionStep _ st = Trampoline.continue <$> handleNextColour (colourAt prog (nextCodelPos mem)) st where
    mem  = st ^. memory
    prog = programMemory mem

handleNextColour ∷ AppSafeEff m ⇒ Maybe Color → AutomatonMemory → m AutomatonMemory
handleNextColour Nothing          st      = pure $ doIfCollided st
handleNextColour (Just Black)     st      = pure $ doIfCollided st
handleNextColour (Just White)     st      = pure $ setPositionState (nextCodelPos mem) 0 st where mem = st ^. memory
handleNextColour (Just (Chromatic c')) st = stepChromatic c' st

stepChromatic ∷ AppSafeEff m ⇒ ChromaticColor → AutomatonMemory → m AutomatonMemory
stepChromatic c' st = evalTransitionBlock (colourAt (programMemory mem) pos) c' block (setPositionState newPos 0 st) where
  mem     = st ^. memory
  pos     = positionMemory mem
  block   = discoverBlock (programMemory mem ^. image) pos
  newPos  = move (directionPointerMemory mem) (selectCodel block mem)

nextCodelPos ∷ Memory → Coordinates
nextCodelPos mem = move (directionPointerMemory mem) (selectCodel block mem) where
  block = discoverBlock (programMemory mem ^. image) pos
  pos   = positionMemory mem

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Block → AutomatonMemory → m AutomatonMemory
evalTransitionBlock (Just (Chromatic c)) c' block st = do
  let blockSize = blockCodelCount block (st ^. memory)
  mem' <- colors2Command c c' blockSize (st ^. memory)
  pure $ st & memory .~ mem'
evalTransitionBlock _ _ _ st = pure st

setPositionState ∷ Coordinates → Int → AutomatonMemory → AutomatonMemory
setPositionState pos cc st = st { _collisionCount = cc } & memory %~ setPosition pos

-- Collision state management

doIfCollided ∷ AutomatonMemory → AutomatonMemory
doIfCollided st = updateCollisionCount $ st & memory %~ handleCollision (even (_collisionCount st))

updateCollisionCount ∷ AutomatonMemory → AutomatonMemory
updateCollisionCount st = st { _collisionCount = _collisionCount st + 1 }
