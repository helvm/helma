module HelVM.HelMA.Automata.WhiteSpace.Parser (
  flipParseVisible,
  flipParseWhite,
  parseVisible,
  parseWhite,
  parse,
  parseTL
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import           HelVM.HelMA.Automata.WhiteSpace.Token


import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.ControlInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction

import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

-- FIXME
flipParseVisible :: Bool -> Source -> Safe InstructionList
flipParseVisible = flip parseVisible

flipParseWhite :: Bool -> Source -> Safe InstructionList
flipParseWhite = flip parseWhite

parseVisible :: Source -> Bool -> Safe InstructionList
parseVisible = parse VisibleTokenType

parseWhite :: Source -> Bool -> Safe InstructionList
parseWhite = parse WhiteTokenType

parse :: MonadSafe m => TokenType -> Source -> Bool -> m InstructionList
parse tokenType = flip parseTL . tokenize tokenType

parseTL :: MonadSafe m => Bool -> TokenList -> m InstructionList
parseTL ascii = go where
  go :: MonadSafe m => TokenList -> m InstructionList
  go []            = pure []
  --IAL instructions
  go (S:S:tl')     = go' =<< parseSymbol tl' where go' (symbol , tl'') = (IAL (Cons symbol)  : ) <$> go tl''
  go (S:T:S:tl')   = go' =<< parseIndex  tl' where go' (index  , tl'') = (IAL (SStatic index Copy)     : ) <$> go tl''
  go (S:T:T:tl')   = panic "STT" tl'
  go (S:T:N:tl')   = go' =<< parseIndex  tl' where go' (index  , tl'') = (IAL (SStatic index Slide)    : ) <$> go tl''
  go (S:N:S:tl')   = (IAL Dup               : ) <$> go tl'
  go (S:N:T:tl')   = (IAL Swap              : ) <$> go tl'
  go (S:N:N:tl')   = (IAL Discard           : ) <$> go tl'
  --Arithmetic
  go (T:S:S:S:tl') = (IAL (Binary Add)      : ) <$> go tl'
  go (T:S:S:T:tl') = (IAL (Binary Sub)      : ) <$> go tl'
  go (T:S:S:N:tl') = (IAL (Binary Mul)      : ) <$> go tl'
  go (T:S:T:S:tl') = (IAL (Binary Div)      : ) <$> go tl'
  go (T:S:T:T:tl') = (IAL (Binary Mod)      : ) <$> go tl'
  go (T:S:T:N:tl') = panic "TSTN" tl'
  go (T:S:N:S:tl') = panic "TSNS" tl'
  go (T:S:N:T:tl') = panic "TSNT" tl'
  go (T:S:N:N:tl') = panic "TSNN" tl'
  --Heap access
  go (T:T:S:tl')   = (ILS Store            : ) <$> go tl'
  go (T:T:T:tl')   = (ILS Load             : ) <$> go tl'
  go (T:T:N:tl')   = panic "TTN" tl'
  --IControl
  go (N:S:S:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (IControl (Mark    label) : ) <$> go tl''
  go (N:S:T:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (IControl (CStatic label Call       ) : ) <$> go tl''
  go (N:S:N:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (IControl (CStatic label Jump       ) : ) <$> go tl''
  go (N:T:S:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (IControl (CStatic label (Branch EZ)) : ) <$> go tl''
  go (N:T:T:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (IControl (CStatic label (Branch LTZ)) : ) <$> go tl''
  go (N:T:N:tl')   = (IControl Return           : ) <$> go tl'
  go (N:N:S:tl')   = panic "NNS" tl'
  go (N:N:T:tl')   = panic "NNT" tl'
  go (N:N:N:tl')   = (End                            : ) <$> go tl'
  --IO instructions
  go (T:N:S:S:tl') = (IAL (SIO OutputChar)        : ) <$> go tl'
  go (T:N:S:T:tl') = (IAL (SIO OutputDec)         : ) <$> go tl'
  go (T:N:S:N:tl') = panic "TNSN" tl'
  go (T:N:T:S:tl') = (ILS (MIO InputChar)         : ) <$> go tl'
  go (T:N:T:T:tl') = (ILS (MIO InputDec)          : ) <$> go tl'
  go (T:N:T:N:tl') = panic "TNTN" tl'
  go (T:N:N:S:tl') = panic "TNNS" tl'
  go (T:N:N:T:tl') = panic "TNNT" tl'
  go (T:N:N:N:tl') = panic "TNNN" tl'
  go tl'           = panic (show tl') []

panic :: MonadSafe m => Text -> TokenList -> m InstructionList
panic token tl = liftErrorTupleList [("Unrecognised tokenl" , token) , ("Rest tokens" , show tl)]
