module HelVM.HelMA.Automata.Piet.Automaton
  ( interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program            as Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline                   as Trampoline

import           HelVM.HelIO.Control.Safe

-- Main interpreter entry point

interpret ∷ AppSafeEff m ⇒ Program → m ()
interpret prog = Trampoline.trampolineM interpretStep initialState where
  initialState = (NormalStep Nothing, initialMemory prog)

interpretStep ∷ AppSafeEff m ⇒ (StepState, Memory) → m (Either () AutomatonMemory)
interpretStep (NormalStep prev, mem) = stepNormal prev mem
interpretStep (WhiteStep limit, mem) = pure $ stepWhite limit mem

-- Step handlers

stepNormal ∷ AppSafeEff m ⇒ Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
stepNormal previous mem = evalPixel (currentPixel mem) previous mem

stepWhite ∷ Int → Memory → Either () AutomatonMemory
stepWhite limit mem
  | limit <= 0 = Trampoline.break ()
  | otherwise  = Trampoline.continue $ checkWhitePixel (currentPixel mem) limit mem

-- Pixel handlers

evalPixel ∷ AppSafeEff m ⇒ Color → Maybe PreviousColor → Memory → m (Either () AutomatonMemory)
evalPixel (Chromatic color) previous mem = evalChromaticPixel previous color mem
evalPixel White             _        mem = pure $ Trampoline.continue $ evalWhitePixel mem
evalPixel Black             _        _   = liftError "Entered black block, terminate"

evalChromaticPixel ∷ AppSafeEff m ⇒ Maybe PreviousColor → ChromaticColor → Memory → m (Either () AutomatonMemory)
evalChromaticPixel previous color mem = makeNext <$> applyPreviousColor previous color mem where
  makeNext mem1 = handleNext (nonBlackSucc (programMemory mem1) mStats (orientationMemory mem1)) (color, getLabelSize mStats) mem1
  mStats   = getMaskInfo mem

evalWhitePixel ∷ Memory → AutomatonMemory
evalWhitePixel mem = (WhiteStep whiteLimit, mem) where
  whiteLimit = 8 * getLabelSize (getMaskInfo mem)

checkWhitePixel ∷ Color → Int → Memory → AutomatonMemory
checkWhitePixel White limit = checkWhitePixelStep limit
checkWhitePixel _     _     = (NormalStep Nothing, )

checkWhitePixelStep ∷ Int → Memory → AutomatonMemory
checkWhitePixelStep limit mem = (WhiteStep (limit - 1), stepWhitePixel mem)

-- Helper functions

handleNext ∷ Maybe InstructionCounter → PreviousColor → Memory → Either () AutomatonMemory
handleNext (Just ic) prevColor mem = Trampoline.continue $ handleNextSuccess ic prevColor mem
handleNext Nothing           _   _ = Trampoline.break ()

handleNextSuccess ∷ InstructionCounter → PreviousColor → Memory → AutomatonMemory
handleNextSuccess ic prevColor mem =
  ( NormalStep (Just prevColor)
  , setInstructionCounter ic mem
  )

data StepState
  = NormalStep (Maybe PreviousColor)
  | WhiteStep Int

type AutomatonMemory = (StepState, Memory)
