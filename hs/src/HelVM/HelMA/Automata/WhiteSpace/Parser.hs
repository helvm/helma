module HelVM.HelMA.Automata.WhiteSpace.Parser (
  flipParseVisible,
  parseVisible,
  parseWhite,
  parse,
  parseTL
) where

import           HelVM.HelMA.Automata.WhiteSpace.Instruction
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.Common.Safe
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.BinaryOperator
import           HelVM.HelMA.Automaton.Types.TokenType

flipParseVisible :: Bool -> Source -> Safe InstructionList
flipParseVisible = flip parseVisible

parseVisible :: Source -> Bool -> Safe InstructionList
parseVisible = parse VisibleTokenType

parseWhite :: Source -> Bool -> Safe InstructionList
parseWhite = parse WhiteTokenType

parse :: TokenType -> Source -> Bool -> Safe InstructionList
parse tokenType = parseTL . tokenize tokenType

parseTL :: TokenList -> Bool -> Safe InstructionList
parseTL tl ascii = go tl where
  go :: TokenList -> Safe InstructionList
  go []            = pure []
  -- | Stack instructions
  go (S:S:tl')     = go' =<< parseSymbol tl' where go' (symbol , tl'') = (Liter symbol  : ) <$> go tl''
  go (S:T:S:tl')   = go' =<< parseIndex  tl' where go' (index  , tl'') = (Copy  index   : ) <$> go tl''
  go (S:T:T:tl')   = panic "STT" tl'
  go (S:T:N:tl')   = go' =<< parseIndex  tl' where go' (index  , tl'') = (Slide  index  : ) <$> go tl''
  go (S:N:S:tl')   = (Dup               : ) <$> go tl'
  go (S:N:T:tl')   = (Swap              : ) <$> go tl'
  go (S:N:N:tl')   = (Discard           : ) <$> go tl'
  --Arithmetic
  go (T:S:S:S:tl') = (Binary Add        : ) <$> go tl'
  go (T:S:S:T:tl') = (Binary Sub        : ) <$> go tl'
  go (T:S:S:N:tl') = (Binary Mul        : ) <$> go tl'
  go (T:S:T:S:tl') = (Binary Div        : ) <$> go tl'
  go (T:S:T:T:tl') = (Binary Mod        : ) <$> go tl'
  go (T:S:T:N:tl') = panic "TSTN" tl'
  go (T:S:N:S:tl') = panic "TSNS" tl'
  go (T:S:N:T:tl') = panic "TSNT" tl'
  go (T:S:N:N:tl') = panic "TSNN" tl'
  -- | Heap access
  go (T:T:S:tl')   = (Store             : ) <$> go tl'
  go (T:T:T:tl')   = (Load              : ) <$> go tl'
  go (T:T:N:tl')   = panic "TTN" tl'
  -- | Control
  go (N:S:S:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (Mark       label : ) <$> go tl''
  go (N:S:T:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (Call       label : ) <$> go tl''
  go (N:S:N:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (Jump       label : ) <$> go tl''
  go (N:T:S:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (Branch EZ  label : ) <$> go tl''
  go (N:T:T:tl')   = go' =<< parseLabel ascii tl' where go' (label  , tl'') = (Branch Neg label : ) <$> go tl''
  go (N:T:N:tl')   = (Return            : ) <$> go tl'
  go (N:N:S:tl')   = panic "NNS" tl'
  go (N:N:T:tl')   = panic "NNT" tl'
  go (N:N:N:tl')   = (End               : ) <$> go tl'
  -- | IO instructions
  go (T:N:S:S:tl') = (OutputChar        : ) <$> go tl'
  go (T:N:S:T:tl') = (OutputNum         : ) <$> go tl'
  go (T:N:S:N:tl') = panic "TNSN" tl'
  go (T:N:T:S:tl') = (InputChar         : ) <$> go tl'
  go (T:N:T:T:tl') = (InputNum          : ) <$> go tl'
  go (T:N:T:N:tl') = panic "TNTN" tl'
  go (T:N:N:S:tl') = panic "TNNS" tl'
  go (T:N:N:T:tl') = panic "TNNT" tl'
  go (T:N:N:N:tl') = panic "TNNN" tl'
  go tl'           = panic (show tl') []

panic :: Text -> TokenList -> Safe InstructionList
panic token tl = liftErrorTupleList [("Unrecognised" , token) , ("Rest" , show tl)]
