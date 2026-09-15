module HelVM.HelMA.Automaton.Automaton
  ( runAutomat
  , start
  ) where

import           HelVM.HelMA.Automaton.API.AutomatonOptions
import           HelVM.HelMA.Automaton.API.AutoOptions

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Symbol
import           HelVM.HelMA.Automaton.Trampoline           as Trampoline

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelMA.Automaton.Combiner
import           HelVM.HelMA.Automaton.Combiner.CPU         as CPU

import qualified HelVM.HelIO.Collections.MapList            as MapList
import qualified HelVM.HelIO.Collections.SList              as SList

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           Control.Monad.Except                       ( catchError, throwError )
import           Control.Monad.Extra

import qualified Data.Sequence                              as Seq

import           Prelude                                    hiding ( swap )

-- PUBLIC API

start ∷ AppSafeEff m ⇒ InstructionList → AutomatonOptions → m ()
start il ao = start' il (stackType ao) (ramType ao) (autoOptions ao)

runAndDumpLogs ∷ (SRAutomatonEff Symbol s r m) ⇒ AutoOptions → Memory s r → m ()
runAndDumpLogs p = logDump (dumpType p) <=< runAutomat (limit p)

runAutomat ∷ (SRAutomatonEff Symbol s r m) ⇒ LimitMaybe → F s r m
runAutomat = trampolineMWithLimit nextState
{-# INLINE runAutomat #-}

-- PRIVATE HELPERS

start' ∷ AppSafeEff m ⇒ InstructionList → StackType → RAMType → AutoOptions → m ()
start' il s ListRAMType    = start'' il s []
start' il s SeqRAMType     = start'' il s Seq.empty
start' il s SListRAMType   = start'' il s SList.sListEmpty
start' il s MapListRAMType = start'' il s MapList.mapListEmpty

start'' ∷ (RAutomatonEff Symbol r m) ⇒ InstructionList → StackType → r → AutoOptions → m ()
start'' il ListStackType  = start''' il []
start'' il SeqStackType   = start''' il Seq.empty
start'' il SListStackType = start''' il SList.sListEmpty

start''' ∷ (SRAutomatonEff Symbol s r m) ⇒ InstructionList → s → r → AutoOptions → m ()
start''' il s r p = runAndDumpLogs p (newMemory il s r)

nextState ∷ (SRAutomatonEff Symbol s r m) ⇒ SF s r m
nextState !a = stepNextState a =<< currentInstruction (memoryCM a)
{-# INLINE nextState #-}

stepNextState ∷ (SRAutomatonEff Symbol s r m) ⇒ Memory s r → Instruction → m (MemorySame s r)
stepNextState !a !i = attachErrorContext a i $ runInstruction i (incrementIC a)
{-# INLINE stepNextState #-}

-- Leniwe dodawanie kontekstu - zapobiega generowaniu ciągów znaków show/printIndexedIL przy poprawnym biegu
attachErrorContext ∷ (SRAutomatonEff Symbol s r m) ⇒ Memory s r → Instruction → m b → m b
attachErrorContext a i action = action `catchError` \err ->
  appendErrorTuple ("Automaton.nextState" , showP a) $
  appendErrorTuple ("program:" , toText $ printIndexedIL $ toList $ memoryProgram a) $
  appendErrorTuple ("i:" , show i) $
  throwError err
{-# INLINE attachErrorContext #-}
