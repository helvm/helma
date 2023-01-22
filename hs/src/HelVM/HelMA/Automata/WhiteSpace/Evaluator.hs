module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Automaton

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automaton.API.AutoParams

import           HelVM.HelIO.Control.Safe

import           Prelude                                      hiding (swap)

import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S

import qualified HelVM.HelIO.Collections.MapList              as MapList
import qualified HelVM.HelIO.Collections.SList                as SList

import qualified Data.Sequence                                as Seq

simpleEval :: BIO m => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.formatType p) (S.stackType p) (S.ramType p) (S.autoParams p)

----

evalParams :: BIO m => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (formatType p) (stackAutoOptions p) (ramAutoOptions p) (autoParams p)

eval :: BIO m => TokenType -> Source -> FormatType -> StackType -> RAMType -> AutoParams -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: BIO m => TokenList -> FormatType -> StackType -> RAMType -> AutoParams -> m ()
evalTL tl ascii st rt p = evalTL' =<< liftSafe (parseFromTL ascii tl) where evalTL' il = evalIL il st rt p

evalIL :: BIO m => InstructionList -> StackType -> RAMType -> AutoParams -> m ()
evalIL il s ListRAMType    = evalIL' il s []
evalIL il s SeqRAMType     = evalIL' il s Seq.empty
evalIL il s SListRAMType   = evalIL' il s SList.sListEmpty
evalIL il s MapListRAMType = evalIL' il s MapList.mapListEmpty

evalIL' :: (RAutomatonIO Symbol r m) => InstructionList -> StackType -> r -> AutoParams -> m ()
evalIL' il ListStackType  = evalIL'' il []
evalIL' il SeqStackType   = evalIL'' il Seq.empty
evalIL' il SListStackType = evalIL'' il SList.sListEmpty

evalIL'' :: (SRAutomatonIO Symbol s r m) => InstructionList -> s -> r -> AutoParams -> m ()
evalIL'' il s r p = runAndDumpLogs p (newAutomaton il s r)
