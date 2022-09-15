module HelVM.HelMA.Automata.WhiteSpace.Parser (
  flipParseVisible,
  flipParseWhite,
  parseVisible,
  parseWhite,
  parse,
  parseFromTL
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import           HelVM.HelMA.Automata.WhiteSpace.Token


import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction
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
parse tokenType = flip parseFromTL . tokenize tokenType

parseFromTL :: MonadSafe m => Bool -> TokenList -> m InstructionList
parseFromTL ascii = go where
  go :: MonadSafe m => TokenList -> m InstructionList
  go      []   = pure              []
  go (S:  tl') = stackManipulation tl'
  go (T:S:tl') = arithmetic        tl'
  go (T:T:tl') = heapAccess        tl'
  go (N  :tl') = flowControl       tl'
  go (T:N:tl') = ioInstruction     tl'
  go      tl'  = notEnoughTokens   tl'

  stackManipulation :: MonadSafe m => TokenList -> m InstructionList
  stackManipulation (S:  tl') = go' =<< parseSymbol tl' where go' (symbol , tl'') = (IAL (Cons symbol)         : ) <$> go tl''
  stackManipulation (T:S:tl') = go' =<< parseIndex  tl' where go' (index  , tl'') = (IAL (SStatic index Copy)  : ) <$> go tl''
  stackManipulation (T:T:tl') = unrecognisedTokens "STT" tl'
  stackManipulation (T:N:tl') = go' =<< parseIndex  tl' where go' (index  , tl'') = (IAL (SStatic index Slide) : ) <$> go tl''
  stackManipulation (N:S:tl') = (IAL Dup     : ) <$> go tl'
  stackManipulation (N:T:tl') = (IAL Swap    : ) <$> go tl'
  stackManipulation (N:N:tl') = (IAL Discard : ) <$> go tl'
  stackManipulation      tl'  = notEnoughTokensForIMP "stackManipulation" tl'

  arithmetic :: MonadSafe m => TokenList -> m InstructionList
  arithmetic (S:S:tl') = (IAL (Binary Add) : ) <$> go tl'
  arithmetic (S:T:tl') = (IAL (Binary Sub) : ) <$> go tl'
  arithmetic (S:N:tl') = (IAL (Binary Mul) : ) <$> go tl'
  arithmetic (T:S:tl') = (IAL (Binary Div) : ) <$> go tl'
  arithmetic (T:T:tl') = (IAL (Binary Mod) : ) <$> go tl'
  arithmetic (T:N:tl') = unrecognisedTokens "TSTN" tl'
  arithmetic (N:S:tl') = unrecognisedTokens "TSNS" tl'
  arithmetic (N:T:tl') = unrecognisedTokens "TSNT" tl'
  arithmetic (N:N:tl') = unrecognisedTokens "TSNN" tl'
  arithmetic      tl'  = notEnoughTokensForIMP "arithmetic" tl'

  heapAccess :: MonadSafe m => TokenList -> m InstructionList
  heapAccess (S:tl') = (ILS Store : ) <$> go tl'
  heapAccess (T:tl') = (ILS Load  : ) <$> go tl'
  heapAccess (N:tl') = unrecognisedTokens "TTN" tl'
  heapAccess    tl'  = notEnoughTokensForIMP "heapAccess" tl'

  flowControl :: MonadSafe m => TokenList -> m InstructionList
  flowControl (S:S:tl') = go' =<< parseLabel ascii tl' where go' (label , tl'') = (ICF (Mark    label             ) : ) <$> go tl''
  flowControl (S:T:tl') = go' =<< parseLabel ascii tl' where go' (label , tl'') = (ICF (CStatic label Call        ) : ) <$> go tl''
  flowControl (S:N:tl') = go' =<< parseLabel ascii tl' where go' (label , tl'') = (ICF (CStatic label Jump        ) : ) <$> go tl''
  flowControl (T:S:tl') = go' =<< parseLabel ascii tl' where go' (label , tl'') = (ICF (CStatic label (Branch EZ )) : ) <$> go tl''
  flowControl (T:T:tl') = go' =<< parseLabel ascii tl' where go' (label , tl'') = (ICF (CStatic label (Branch LTZ)) : ) <$> go tl''
  flowControl (T:N:tl') = (ICF Return : ) <$> go tl'
  flowControl (N:S:tl') = unrecognisedTokens "NNS" tl'
  flowControl (N:T:tl') = unrecognisedTokens "NNT" tl'
  flowControl (N:N:tl') = (End             : ) <$> go tl'
  flowControl      tl'  = notEnoughTokensForIMP "flowControl" tl'

  ioInstruction :: MonadSafe m => TokenList -> m InstructionList
  ioInstruction (S:S:tl') = (IAL (SIO OutputChar) : ) <$> go tl'
  ioInstruction (S:T:tl') = (IAL (SIO OutputDec)  : ) <$> go tl'
  ioInstruction (S:N:tl') = unrecognisedTokens "TNSN" tl'
  ioInstruction (T:S:tl') = (ILS (MIO InputChar)  : ) <$> go tl'
  ioInstruction (T:T:tl') = (ILS (MIO InputDec)   : ) <$> go tl'
  ioInstruction (T:N:tl') = unrecognisedTokens "TNTN" tl'
  ioInstruction (N:S:tl') = unrecognisedTokens "TNNS" tl'
  ioInstruction (N:T:tl') = unrecognisedTokens "TNNT" tl'
  ioInstruction (N:N:tl') = unrecognisedTokens "TNNN" tl'
  ioInstruction tl'       = notEnoughTokensForIMP "ioInstruction" tl'

unrecognisedTokens :: MonadSafe m => Text -> TokenList -> m InstructionList
unrecognisedTokens tokens tl = liftErrorTupleList [("Unrecognised tokens" , tokens) , ("Rest tokens" , show tl)]

notEnoughTokensForIMP :: MonadSafe m => Text -> TokenList -> m InstructionList
notEnoughTokensForIMP imp tl = liftErrorTupleList [("Not enough tokens for IMP:" , imp) , ("Rest tokens" , show tl)]

notEnoughTokens :: MonadSafe m => TokenList -> m InstructionList
notEnoughTokens tl = liftErrorTupleList [("Rest tokens" , show tl)]
