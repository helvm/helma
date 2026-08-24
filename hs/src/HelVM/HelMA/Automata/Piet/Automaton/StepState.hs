module HelVM.HelMA.Automata.Piet.Automaton.StepState
  ( memory
  , start
  , stepState
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program            as Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline                   as Trampoline

import           HelVM.HelIO.Control.Safe

import           Lens.Micro.Platform

-- TYPES & LENSES

data StepState
  = ChromaticStep (Maybe PreviousColor)
  | WhiteStep {-# UNPACK #-} !Int

data AutomatonMemory
  = AutomatonMemory
      { _stepState :: !StepState
      , _memory    :: !Memory
      }

makeLenses ''AutomatonMemory

-- MAIN INTERPRETER ENTRY POINT

initialState ∷ Program → AutomatonMemory
initialState prog = AutomatonMemory
  { _stepState = ChromaticStep Nothing
  , _memory    = initialMemory prog
  }

start ∷ AppSafeEff m ⇒ Program → m ()
start = Trampoline.trampolineM interpretStep . initialState

interpretStep ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
interpretStep (AutomatonMemory (ChromaticStep prev) mem) = stepChromatic prev mem
interpretStep (AutomatonMemory (WhiteStep limit)    mem) = pure $ stepWhite limit mem

-- STEP HANDLERS

stepChromatic ∷ AppSafeEff m ⇒ Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
stepChromatic prev mem = evalPixel (currentPixel mem) prev mem

{-# INLINE stepWhite #-}
stepWhite ∷ Int → Memory → Either () AutomatonMemory
stepWhite limit mem
  | limit <= 0 = Trampoline.break ()
  | otherwise  = Trampoline.continue $ checkWhitePixel (currentPixel mem) limit mem

-- PIXEL HANDLERS

evalPixel ∷ AppSafeEff m ⇒ Color → Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
evalPixel (Chromatic color) prev mem = evalChromaticPixel color prev mem
evalPixel White             _    mem = pure $ Trampoline.continue $ evalWhitePixel mem
evalPixel Black             _    _   = liftError "Entered black block, terminate"

evalChromaticPixel ∷ AppSafeEff m ⇒ ChromaticColor → Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
evalChromaticPixel color previous mem = makeNext <$> applyPreviousColor previous color mem where
  makeNext mem' = handleNext (nonBlackSuccMemory mem' mStats) color (blockCodelCount mem') mem'
  mStats        = getMaskInfo mem

-- Używamy z powrotem mStats (z gotowej maski O(1)), zamiast powolnego re-discovery bloku!
evalWhitePixel ∷ Memory → AutomatonMemory
evalWhitePixel mem = AutomatonMemory
  { _stepState = WhiteStep (8 * getLabelSize (getMaskInfo mem))
  , _memory    = mem
  }

checkWhitePixel ∷ Color → Int → Memory → AutomatonMemory
checkWhitePixel White limit mem = AutomatonMemory (WhiteStep (limit - 1)) (stepWhitePixel mem)
checkWhitePixel _     _     mem = AutomatonMemory (ChromaticStep Nothing) mem

-- HELPER FUNCTIONS

{-# INLINE handleNext #-}
handleNext ∷ Maybe InstructionCounter → ChromaticColor → Int → Memory → Either () AutomatonMemory
handleNext (Just ic) color count mem = Trampoline.continue $ AutomatonMemory
  { _stepState = ChromaticStep (Just (color, count))
  , _memory    = setInstructionCounter ic mem
  }
handleNext Nothing _ _ _ = Trampoline.break ()
