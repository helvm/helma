module HelVM.HelMA.Automata.BrainFuck.Lexer where

import           HelVM.HelMA.Automata.BrainFuck.Instruction.FlatInstruction

import           HelVM.Common.ReadText
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.WrapTokenList

-- | Lexer
tokenize :: Source -> FlatTreeInstructionList
tokenize =  unWrapTokenList . readTokens

readTokens :: Source -> Tokens
readTokens source = readTextUnsafe source :: Tokens

type Tokens = WrapTokenList FlatTreeInstructionList
