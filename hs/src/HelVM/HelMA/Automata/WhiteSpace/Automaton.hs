module HelVM.HelMA.Automata.WhiteSpace.Automaton (
  simpleRun,
  runWithParams,
  run,
  runIL,
  runTL,
  start,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import           HelVM.HelMA.Automata.WhiteSpace.Symbol
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Units.CPU              as CPU

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

import           Prelude                                      hiding (swap)

import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S

import qualified HelVM.HelIO.Collections.MapList              as MapList
import qualified HelVM.HelIO.Collections.SList                as SList

import qualified Data.Sequence                                as Seq
import qualified Data.Vector                                  as Vector

simpleRun :: BIO m => S.SimpleParams -> m ()
simpleRun p = run (S.tokenType p) (S.source p) (S.asciiLabel p) (S.stackType p) (S.ramType p) (S.dumpType p)

----

runWithParams :: BIO m => TokenType -> RunParams -> m ()
runWithParams tokenType p = run tokenType (source p) (asciiLabel p) (stackTypeOptions p) (ramTypeOptions p) (dumpTypeOptions p)

run :: BIO m => TokenType -> Source -> Bool -> StackType -> RAMType -> DumpType -> m ()
run tokenType source = runTL $ tokenize tokenType source

runTL :: BIO m => TokenList -> Bool -> StackType -> RAMType -> DumpType -> m ()
runTL tl ascii st rt dt = runTL' =<< liftSafe (parseTL ascii tl) where runTL' il = runIL il st rt dt

runIL :: BIO m => InstructionList -> StackType -> RAMType -> DumpType -> m ()
runIL il s ListRAMType    = runIL' il s []
runIL il s SeqRAMType     = runIL' il s Seq.empty
runIL il s SListRAMType   = runIL' il s SList.sListEmpty
runIL il s MapListRAMType = runIL' il s MapList.mapListEmpty

runIL' :: (REvaluator Symbol r m) => InstructionList -> StackType -> r -> DumpType -> m ()
runIL' il ListStackType  = start il []
runIL' il SeqStackType   = start il Seq.empty
runIL' il SListStackType = start il SList.sListEmpty

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> DumpType -> m ()
start il s r dt = logDump dt =<< next (CU (Vector.fromList il) 0 (IS [])) s r
