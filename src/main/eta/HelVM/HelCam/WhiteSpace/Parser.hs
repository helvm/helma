module HelVM.HelCam.WhiteSpace.Parser where

import HelVM.HelCam.WhiteSpace.Token
import HelVM.HelCam.WhiteSpace.Lexer
import HelVM.HelCam.WhiteSpace.Instruction
import HelVM.HelCam.Common.Util

parseWS :: Bool -> Source -> InstructionList
parseWS ascii = parseWSTL ascii . tokenizeWS

parseWSTL :: Bool -> TokenList -> InstructionList
parseWSTL ascii = parse where
  parse :: TokenList -> InstructionList
  parse []               = []
  -- Stack instructions
  parse (S:S:tokens)     = Const value            : parse tokens' where (value, tokens') = parseValue tokens
  parse (S:T:S:tokens)   = Ref   index            : parse tokens' where (index, tokens') = parseIndex tokens
  parse (S:T:T:_)        = panic "STT"
  parse (S:T:N:tokens)   = Slide index            : parse tokens' where (index, tokens') = parseIndex tokens
  parse (S:N:S:tokens)   = Dup                    : parse tokens
  parse (S:N:T:tokens)   = Swap                   : parse tokens
  parse (S:N:N:tokens)   = Discard                : parse tokens
  --Arithmetic
  parse (T:S:S:S:tokens) = Binary Add             : parse tokens
  parse (T:S:S:T:tokens) = Binary Sub             : parse tokens
  parse (T:S:S:N:tokens) = Binary Mul             : parse tokens
  parse (T:S:T:S:tokens) = Binary Div             : parse tokens
  parse (T:S:T:T:tokens) = Binary Mod             : parse tokens
  parse (T:S:T:N:_)      = panic "TSTN"
  parse (T:S:N:S:_)      = panic "TSNS"
  parse (T:S:N:T:_)      = panic "TSNT"
  parse (T:S:N:N:_)      = panic "TSNN"
  -- Heap access
  parse (T:T:S:tokens)   = Store                  : parse tokens
  parse (T:T:T:tokens)   = Load                   : parse tokens
  parse (T:T:N:_)        = panic "TTN"
  -- Control
  parse (N:S:S:tokens)   = Label       identifier : parse tokens' where (identifier, tokens') = parseIdentifier ascii tokens
  parse (N:S:T:tokens)   = Call        identifier : parse tokens' where (identifier, tokens') = parseIdentifier ascii tokens
  parse (N:S:N:tokens)   = Jump        identifier : parse tokens' where (identifier, tokens') = parseIdentifier ascii tokens
  parse (N:T:S:tokens)   = Branch EZ   identifier : parse tokens' where (identifier, tokens') = parseIdentifier ascii tokens
  parse (N:T:T:tokens)   = Branch Neg  identifier : parse tokens' where (identifier, tokens') = parseIdentifier ascii tokens
  parse (N:T:N:tokens)   = Return                 : parse tokens
  parse (N:N:S:_)        = panic "NNS"
  parse (N:N:T:_)        = panic "NNT"
  parse (N:N:N:tokens)   = End                    : parse tokens
  -- IO instructions
  parse (T:N:S:S:tokens) = OutputChar             : parse tokens
  parse (T:N:S:T:tokens) = OutputNum              : parse tokens
  parse (T:N:S:N:_)      = panic "TNSN"
  parse (T:N:T:S:tokens) = InputChar              : parse tokens
  parse (T:N:T:T:tokens) = InputNum               : parse tokens
  parse (T:N:T:N:_)      = panic "TNTN"
  parse (T:N:N:S:_)      = panic "TNNS"
  parse (T:N:N:T:_)      = panic "TNNT"
  parse (T:N:N:N:_)      = panic "TNNN"
  parse tokens           = panic $ show tokens

panic :: String -> InstructionList
panic token = error $ "Unrecognised " ++ token
