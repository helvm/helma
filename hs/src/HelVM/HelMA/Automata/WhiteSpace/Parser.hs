module HelVM.HelMA.Automata.WhiteSpace.Parser (
  parseForTest,
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
import           HelVM.HelMA.Automaton.Instruction.Extras.Constructors

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

parseForTest :: FormatType -> TokenType -> Source -> Safe InstructionList
parseForTest formatType tokenType s = parse tokenType s formatType

flipParseVisible :: FormatType -> Source -> Safe InstructionList
flipParseVisible = flip parseVisible

flipParseWhite :: FormatType -> Source -> Safe InstructionList
flipParseWhite = flip parseWhite

parseVisible :: Source -> FormatType -> Safe InstructionList
parseVisible = parse VisibleTokenType

parseWhite :: Source -> FormatType -> Safe InstructionList
parseWhite = parse WhiteTokenType

parse :: MonadSafe m => TokenType -> Source -> FormatType -> m InstructionList
parse tokenType = flip parseFromTL . tokenize tokenType

parseFromTL :: MonadSafe m => FormatType -> TokenList -> m InstructionList
parseFromTL ascii = repeatedlyM (parseInstruction ascii)

parseInstruction :: MonadSafe m => FormatType -> InstructionParser m
parseInstruction     _ (S :     tl) = parseInstructionStackManipulation tl
parseInstruction     _ (T : S : tl) = parseInstructionArithmetic        tl
parseInstruction     _ (T : T : tl) = parseInstructionHeadAccess        tl
parseInstruction ascii (N :     tl) = parseInstructionFlowControl ascii tl
parseInstruction     _ (T : N : tl) = parseInstructionIO                tl
parseInstruction     _          tl  = unrecognisedTokensIn "parseInstruction" tl

parseInstructionStackManipulation :: MonadSafe m => InstructionParser m
parseInstructionStackManipulation (S :     tl) = build <$> parseSymbol tl where build (symbol , tl') = (consI symbol  , tl')
parseInstructionStackManipulation (T : S : tl) = build <$> parseIndex  tl where build (index  , tl') = (copyII  index , tl')
parseInstructionStackManipulation (T : N : tl) = build <$> parseIndex  tl where build (index  , tl') = (slideII index , tl')
parseInstructionStackManipulation (N : S : tl) = pure (dupI     , tl)
parseInstructionStackManipulation (N : T : tl) = pure (swapI    , tl)
parseInstructionStackManipulation (N : N : tl) = pure (discardI , tl)
parseInstructionStackManipulation          tl  = unrecognisedTokensIn "parseInstructionStackManipulation" tl

parseInstructionArithmetic :: MonadSafe m => InstructionParser m
parseInstructionArithmetic (S : S : tl) = pure (addI , tl)
parseInstructionArithmetic (S : T : tl) = pure (subI , tl)
parseInstructionArithmetic (S : N : tl) = pure (mulI , tl)
parseInstructionArithmetic (T : S : tl) = pure (divI , tl)
parseInstructionArithmetic (T : T : tl) = pure (modI , tl)
parseInstructionArithmetic          tl  = unrecognisedTokensIn "parseInstructionArithmetic" tl

parseInstructionHeadAccess :: MonadSafe m => InstructionParser m
parseInstructionHeadAccess (S : tl) = pure (storeI , tl)
parseInstructionHeadAccess (T : tl) = pure (loadI  , tl)
parseInstructionHeadAccess      tl  = unrecognisedTokensIn "parseInstructionHeadAccess" tl

parseInstructionFlowControl :: MonadSafe m => FormatType -> InstructionParser m
parseInstructionFlowControl ascii (S : S : tl) = build <$> parseLabel ascii tl where build (label , tl') = (markSI  label , tl')
parseInstructionFlowControl ascii (S : T : tl) = build <$> parseLabel ascii tl where build (label , tl') = (callSI label  , tl')
parseInstructionFlowControl ascii (S : N : tl) = build <$> parseLabel ascii tl where build (label , tl') = (jumpSI label  , tl')
parseInstructionFlowControl ascii (T : S : tl) = build <$> parseLabel ascii tl where build (label , tl') = (bEzSI   label  , tl')
parseInstructionFlowControl ascii (T : T : tl) = build <$> parseLabel ascii tl where build (label , tl') = (bLtzSI  label  , tl')
parseInstructionFlowControl     _ (T : N : tl) = pure (returnI , tl)
parseInstructionFlowControl     _ (N : N : tl) = pure (End     , tl)
parseInstructionFlowControl     _          tl  = unrecognisedTokensIn "parseInstructionFlowControl" tl

parseInstructionIO :: MonadSafe m => InstructionParser m
parseInstructionIO (S : S : tl) = pure (sOutputI    , tl)
parseInstructionIO (S : T : tl) = pure (sOutputDecI , tl)
parseInstructionIO (T : S : tl) = pure (mInputI     , tl)
parseInstructionIO (T : T : tl) = pure (mInputDecI  , tl)
parseInstructionIO          tl  = unrecognisedTokensIn "parseInstructionIO" tl

unrecognisedTokensIn :: MonadSafe m => Text -> TokenList -> m a
unrecognisedTokensIn name tl = liftErrorTupleList [("Unrecognised tokens in" , name) , ("Rest tokens" , show tl)]

type InstructionParser m = ParserFromTokenList m Instruction
