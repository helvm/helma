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
import           HelVM.HelIO.Extra

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
parseFromTL ascii = repeatedlyM (parseInstruction ascii)

parseInstruction :: MonadSafe m => Bool -> InstructionParser m
parseInstruction     _ (S :     tl) = parseInstructionStackManipulation tl
parseInstruction     _ (T : S : tl) = parseInstructionArithmetic        tl
parseInstruction     _ (T : T : tl) = parseInstructionHeadAccess        tl
parseInstruction ascii (N :     tl) = parseInstructionFlowControl ascii tl
parseInstruction     _ (T : N : tl) = parseInstructionIO                tl
parseInstruction     _          tl  = unrecognisedTokensIn "parseInstruction" tl

parseInstructionStackManipulation :: MonadSafe m => InstructionParser m
parseInstructionStackManipulation (S :     tl) = build <$> parseSymbol tl where build (symbol , tl') = (IAL (Cons    symbol     ) , tl')
parseInstructionStackManipulation (T : S : tl) = build <$> parseIndex  tl where build (index  , tl') = (IAL (SStatic index Copy ) , tl')
parseInstructionStackManipulation (T : N : tl) = build <$> parseIndex  tl where build (index  , tl') = (IAL (SStatic index Slide) , tl')
parseInstructionStackManipulation (N : S : tl) = pure (IAL dupI    , tl)
parseInstructionStackManipulation (N : T : tl) = pure (IAL swapI   , tl)
parseInstructionStackManipulation (N : N : tl) = pure (IAL Discard , tl)
parseInstructionStackManipulation          tl  = unrecognisedTokensIn "parseInstructionStackManipulation" tl

parseInstructionArithmetic :: MonadSafe m => InstructionParser m
parseInstructionArithmetic (S : S : tl) = pure (IAL (Binary Add) , tl)
parseInstructionArithmetic (S : T : tl) = pure (IAL (Binary Sub) , tl)
parseInstructionArithmetic (S : N : tl) = pure (IAL (Binary Mul) , tl)
parseInstructionArithmetic (T : S : tl) = pure (IAL (Binary Div) , tl)
parseInstructionArithmetic (T : T : tl) = pure (IAL (Binary Mod) , tl)
parseInstructionArithmetic          tl  = unrecognisedTokensIn "parseInstructionArithmetic" tl

parseInstructionHeadAccess :: MonadSafe m => InstructionParser m
parseInstructionHeadAccess (S : tl) = pure (ILS Store , tl)
parseInstructionHeadAccess (T : tl) = pure (ILS Load  , tl)
parseInstructionHeadAccess      tl  = unrecognisedTokensIn "parseInstructionHeadAccess" tl

parseInstructionFlowControl :: MonadSafe m => Bool -> InstructionParser m
parseInstructionFlowControl ascii (S : S : tl) = build <$> parseLabel ascii tl where build (label , tl') = (ICF (Mark    label             ) , tl')
parseInstructionFlowControl ascii (S : T : tl) = build <$> parseLabel ascii tl where build (label , tl') = (ICF (CStatic label  Call       ) , tl')
parseInstructionFlowControl ascii (S : N : tl) = build <$> parseLabel ascii tl where build (label , tl') = (ICF (CStatic label  Jump       ) , tl')
parseInstructionFlowControl ascii (T : S : tl) = build <$> parseLabel ascii tl where build (label , tl') = (ICF (CStatic label (Branch EZ )) , tl')
parseInstructionFlowControl ascii (T : T : tl) = build <$> parseLabel ascii tl where build (label , tl') = (ICF (CStatic label (Branch LTZ)) , tl')
parseInstructionFlowControl     _ (T : N : tl) = pure (ICF Return , tl)
parseInstructionFlowControl     _ (N : N : tl) = pure (End        , tl)
parseInstructionFlowControl     _          tl  = unrecognisedTokensIn "parseInstructionFlowControl" tl

parseInstructionIO :: MonadSafe m => InstructionParser m
parseInstructionIO (S : S : tl) = pure (IAL (SIO OutputChar) , tl)
parseInstructionIO (S : T : tl) = pure (IAL (SIO OutputDec ) , tl)
parseInstructionIO (T : S : tl) = pure (ILS (MIO InputChar ) , tl)
parseInstructionIO (T : T : tl) = pure (ILS (MIO InputDec  ) , tl)
parseInstructionIO          tl  = unrecognisedTokensIn "parseInstructionIO" tl

unrecognisedTokensIn :: MonadSafe m => Text -> TokenList -> m a
unrecognisedTokensIn name tl = liftErrorTupleList [("Unrecognised tokens in" , name) , ("Rest tokens" , show tl)]

type InstructionParser m = ParserFromTokenList m Instruction
