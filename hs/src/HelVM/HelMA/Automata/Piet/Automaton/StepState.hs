module HelVM.HelMA.Automata.Piet.Automaton.StepState
  ( memoryL
  , start
  , stepStateL
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

import           Relude.Extra

-- TYPES & LENSES

data StepState
  = ChromaticStep (Maybe PreviousColor)
  | WhiteStep {-# UNPACK #-} !Int

data AutomatonMemory
  = AutomatonMemory
      { stepState :: !StepState
      , memory    :: !Memory
      }

stepStateL ∷ Lens' AutomatonMemory StepState
stepStateL = lens stepState (\s x -> s { stepState = x })

memoryL ∷ Lens' AutomatonMemory Memory
memoryL = lens memory (\s x -> s { memory = x })

-- MAIN INTERPRETER ENTRY POINT

initialState ∷ Program → AutomatonMemory
initialState prog = AutomatonMemory
  { stepState = ChromaticStep Nothing
  , memory    = initialMemory prog
  }

start ∷ AppSafeEff m ⇒ Program → m ()
start = Trampoline.trampolineM interpretStep . initialState

interpretStep ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
interpretStep (AutomatonMemory (ChromaticStep p) mem) = stepChromatic p mem
interpretStep (AutomatonMemory (WhiteStep limit) mem) = pure $ stepWhite limit mem

-- STEP HANDLERS

stepChromatic ∷ AppSafeEff m ⇒ Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
stepChromatic p mem = evalPixel (currentPixel mem) p mem

{-# INLINE stepWhite #-}
stepWhite ∷ Int → Memory → Either () AutomatonMemory
stepWhite limit mem
  | limit <= 0 = Trampoline.break ()
  | otherwise  = Trampoline.continue $ checkWhitePixel (currentPixel mem) limit mem

-- PIXEL HANDLERS

evalPixel ∷ AppSafeEff m ⇒ Color → Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
evalPixel (Chromatic color) p m = evalChromaticPixel color p m
evalPixel White             _ m = pure $ Trampoline.continue $ evalWhitePixel m
evalPixel Black             _ _ = liftError "Entered black block, terminate"

evalChromaticPixel ∷ AppSafeEff m ⇒ ChromaticColor → Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
evalChromaticPixel color previous mem = makeNext <$> applyPreviousColor previous color mem where
  makeNext mem' = handleNext (nonBlackSuccMemory mem' mStats) color (blockCodelCount mem') mem'
  mStats        = getMaskInfo mem

-- Używamy z powrotem mStats (z gotowej maski O(1)), zamiast powolnego re-discovery bloku!
evalWhitePixel ∷ Memory → AutomatonMemory
evalWhitePixel mem = AutomatonMemory
  { stepState = WhiteStep (8 * getLabelSize (getMaskInfo mem))
  , memory    = mem
  }

checkWhitePixel ∷ Color → Int → Memory → AutomatonMemory
checkWhitePixel White limit mem = AutomatonMemory (WhiteStep (limit - 1)) (stepWhitePixel mem)
checkWhitePixel _     _     mem = AutomatonMemory (ChromaticStep Nothing) mem

-- HELPER FUNCTIONS

{-# INLINE handleNext #-}
handleNext ∷ Maybe InstructionCounter → ChromaticColor → Int → Memory → Either () AutomatonMemory
handleNext (Just ic) color count mem = Trampoline.continue $ AutomatonMemory
  { stepState = ChromaticStep (Just (color, count))
  , memory    = setInstructionCounter ic mem
  }
handleNext Nothing _ _ _ = Trampoline.break ()
