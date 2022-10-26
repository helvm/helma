module HelVM.HelMA.Automata.BrainFuck.Flat.Parser where

import           HelVM.HelMA.Automata.BrainFuck.Flat.Instruction

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.WrapTokenList

import           HelVM.HelIO.ReadText

-- | Parser
tokenize :: Source -> FlatTreeInstructionList
tokenize =  unWrapTokenList . readTokens

readTokens :: Source -> Tokens
readTokens source = readTextUnsafe source :: Tokens

type Tokens = WrapTokenList FlatTreeInstructionList
