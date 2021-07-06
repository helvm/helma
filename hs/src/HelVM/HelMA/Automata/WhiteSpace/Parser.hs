module HelVM.HelMA.Automata.WhiteSpace.Parser (
  flipParseVisible,
  parseVisible,
  parseWhite,
  parse,
  parseTL
) where

import HelVM.HelMA.Automata.WhiteSpace.Token
import HelVM.HelMA.Automata.WhiteSpace.Lexer
import HelVM.HelMA.Automata.WhiteSpace.Instruction

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.BinaryOperator
import HelVM.Common.Safe
import HelVM.HelMA.Automaton.Types.TokenType

flipParseVisible :: Bool -> Source -> Safe InstructionList
flipParseVisible = flip parseVisible

parseVisible :: Source -> Bool -> Safe InstructionList
parseVisible = parse VisibleTokenType

parseWhite :: Source -> Bool -> Safe InstructionList
parseWhite = parse WhiteTokenType

parse :: TokenType -> Source -> Bool -> Safe InstructionList
parse tokenType source = parseTL $ tokenize tokenType source

parseTL :: TokenList -> Bool -> Safe InstructionList
parseTL tl ascii = parseTL' tl where
  parseTL' :: TokenList -> Safe InstructionList
  parseTL' []            = pure []
  -- Stack instructions
  parseTL' (S:S:tl')     = parseTL'' =<< parseSymbol tl' where parseTL'' (symbol , tl'') = (Liter symbol  : ) <$> parseTL' tl''
  parseTL' (S:T:S:tl')   = parseTL'' =<< parseIndex  tl' where parseTL'' (index  , tl'') = (Copy  index   : ) <$> parseTL' tl''
  parseTL' (S:T:T:tl')   = panic "STT" tl'
  parseTL' (S:T:N:tl')   = parseTL'' =<< parseIndex  tl' where parseTL'' (index  , tl'') = (Slide  index  : ) <$> parseTL' tl''
  parseTL' (S:N:S:tl')   = (Dup               : ) <$> parseTL' tl'
  parseTL' (S:N:T:tl')   = (Swap              : ) <$> parseTL' tl'
  parseTL' (S:N:N:tl')   = (Discard           : ) <$> parseTL' tl'
  --Arithmetic
  parseTL' (T:S:S:S:tl') = (Binary Add        : ) <$> parseTL' tl'
  parseTL' (T:S:S:T:tl') = (Binary Sub        : ) <$> parseTL' tl'
  parseTL' (T:S:S:N:tl') = (Binary Mul        : ) <$> parseTL' tl'
  parseTL' (T:S:T:S:tl') = (Binary Div        : ) <$> parseTL' tl'
  parseTL' (T:S:T:T:tl') = (Binary Mod        : ) <$> parseTL' tl'
  parseTL' (T:S:T:N:tl') = panic "TSTN" tl'
  parseTL' (T:S:N:S:tl') = panic "TSNS" tl'
  parseTL' (T:S:N:T:tl') = panic "TSNT" tl'
  parseTL' (T:S:N:N:tl') = panic "TSNN" tl'
  -- Heap access
  parseTL' (T:T:S:tl')   = (Store             : ) <$> parseTL' tl'
  parseTL' (T:T:T:tl')   = (Load              : ) <$> parseTL' tl'
  parseTL' (T:T:N:tl')   = panic "TTN" tl'
  -- Control
  parseTL' (N:S:S:tl')   = parseTL'' =<< parseLabel ascii tl' where parseTL'' (label  , tl'') = (Mark       label : ) <$> parseTL' tl''
  parseTL' (N:S:T:tl')   = parseTL'' =<< parseLabel ascii tl' where parseTL'' (label  , tl'') = (Call       label : ) <$> parseTL' tl''
  parseTL' (N:S:N:tl')   = parseTL'' =<< parseLabel ascii tl' where parseTL'' (label  , tl'') = (Jump       label : ) <$> parseTL' tl''
  parseTL' (N:T:S:tl')   = parseTL'' =<< parseLabel ascii tl' where parseTL'' (label  , tl'') = (Branch EZ  label : ) <$> parseTL' tl''
  parseTL' (N:T:T:tl')   = parseTL'' =<< parseLabel ascii tl' where parseTL'' (label  , tl'') = (Branch Neg label : ) <$> parseTL' tl''
  parseTL' (N:T:N:tl')   = (Return            : ) <$> parseTL' tl'
  parseTL' (N:N:S:tl')   = panic "NNS" tl'
  parseTL' (N:N:T:tl')   = panic "NNT" tl'
  parseTL' (N:N:N:tl')   = (End               : ) <$> parseTL' tl'
  -- IO instructions
  parseTL' (T:N:S:S:tl') = (OutputChar        : ) <$> parseTL' tl'
  parseTL' (T:N:S:T:tl') = (OutputNum         : ) <$> parseTL' tl'
  parseTL' (T:N:S:N:tl') = panic "TNSN" tl'
  parseTL' (T:N:T:S:tl') = (InputChar         : ) <$> parseTL' tl'
  parseTL' (T:N:T:T:tl') = (InputNum          : ) <$> parseTL' tl'
  parseTL' (T:N:T:N:tl') = panic "TNTN" tl'
  parseTL' (T:N:N:S:tl') = panic "TNNS" tl'
  parseTL' (T:N:N:T:tl') = panic "TNNT" tl'
  parseTL' (T:N:N:N:tl') = panic "TNNN" tl'
  parseTL' tl'           = panic (show tl') []

panic :: Text -> TokenList -> Safe InstructionList
panic token tl = liftErrorTupleList [("Unrecognised" , token) , ("Rest" , show tl)]
