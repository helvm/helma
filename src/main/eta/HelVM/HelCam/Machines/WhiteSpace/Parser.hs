module HelVM.HelCam.Machines.WhiteSpace.Parser where

import HelVM.HelCam.Machines.WhiteSpace.Token
import HelVM.HelCam.Machines.WhiteSpace.Lexer
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Common.Util

parse :: Bool -> Source -> InstructionList
parse ascii = parseTL ascii . tokenize

parseTL :: Bool -> TokenList -> InstructionList
parseTL ascii = parseTL' where
  parseTL' :: TokenList -> InstructionList
  parseTL' []               = []
  -- Stack instructions
  parseTL' (S:S:tokens)     = Liter symbol      : parseTL' tokens' where (symbol, tokens') = parseSymbol tokens
  parseTL' (S:T:S:tokens)   = Copy  index       : parseTL' tokens' where (index, tokens') = parseIndex tokens
  parseTL' (S:T:T:_)        = panic "STT"
  parseTL' (S:T:N:tokens)   = Slide index       : parseTL' tokens' where (index, tokens') = parseIndex tokens
  parseTL' (S:N:S:tokens)   = Dup               : parseTL' tokens
  parseTL' (S:N:T:tokens)   = Swap              : parseTL' tokens
  parseTL' (S:N:N:tokens)   = Discard           : parseTL' tokens
  --Arithmetic
  parseTL' (T:S:S:S:tokens) = Binary Add        : parseTL' tokens
  parseTL' (T:S:S:T:tokens) = Binary Sub        : parseTL' tokens
  parseTL' (T:S:S:N:tokens) = Binary Mul        : parseTL' tokens
  parseTL' (T:S:T:S:tokens) = Binary Div        : parseTL' tokens
  parseTL' (T:S:T:T:tokens) = Binary Mod        : parseTL' tokens
  parseTL' (T:S:T:N:_)      = panic "TSTN"
  parseTL' (T:S:N:S:_)      = panic "TSNS"
  parseTL' (T:S:N:T:_)      = panic "TSNT"
  parseTL' (T:S:N:N:_)      = panic "TSNN"
  -- Heap access
  parseTL' (T:T:S:tokens)   = Store             : parseTL' tokens
  parseTL' (T:T:T:tokens)   = Load              : parseTL' tokens
  parseTL' (T:T:N:_)        = panic "TTN"
  -- Control
  parseTL' (N:S:S:tokens)   = Mark        label : parseTL' tokens' where (label, tokens') = parseLabel ascii tokens
  parseTL' (N:S:T:tokens)   = Call        label : parseTL' tokens' where (label, tokens') = parseLabel ascii tokens
  parseTL' (N:S:N:tokens)   = Jump        label : parseTL' tokens' where (label, tokens') = parseLabel ascii tokens
  parseTL' (N:T:S:tokens)   = Branch EZ   label : parseTL' tokens' where (label, tokens') = parseLabel ascii tokens
  parseTL' (N:T:T:tokens)   = Branch Neg  label : parseTL' tokens' where (label, tokens') = parseLabel ascii tokens
  parseTL' (N:T:N:tokens)   = Return            : parseTL' tokens
  parseTL' (N:N:S:_)        = panic "NNS"
  parseTL' (N:N:T:_)        = panic "NNT"
  parseTL' (N:N:N:tokens)   = End               : parseTL' tokens
  -- IO instructions
  parseTL' (T:N:S:S:tokens) = OutputChar        : parseTL' tokens
  parseTL' (T:N:S:T:tokens) = OutputNum         : parseTL' tokens
  parseTL' (T:N:S:N:_)      = panic "TNSN"
  parseTL' (T:N:T:S:tokens) = InputChar         : parseTL' tokens
  parseTL' (T:N:T:T:tokens) = InputNum          : parseTL' tokens
  parseTL' (T:N:T:N:_)      = panic "TNTN"
  parseTL' (T:N:N:S:_)      = panic "TNNS"
  parseTL' (T:N:N:T:_)      = panic "TNNT"
  parseTL' (T:N:N:N:_)      = panic "TNNN"
  parseTL' tokens           = panic $ show tokens

panic :: Text -> InstructionList
panic token = error $ "Unrecognised " <> token
