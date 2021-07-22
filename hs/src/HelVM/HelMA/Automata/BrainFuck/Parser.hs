module HelVM.HelMA.Automata.BrainFuck.Parser where

import           HelVM.HelMA.Automata.BrainFuck.Instruction as I
import           HelVM.HelMA.Automata.BrainFuck.Lexer
import           HelVM.HelMA.Automata.BrainFuck.Token       as T

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.Common.ListLikeUtil
import           HelVM.Common.Safe

import           Data.ListLike                              hiding (show)

import qualified Data.DList                                 as D

type OperandParser a = TokenList -> Safe (a , TokenList)

parseAsVector :: MonadSafeError m => Source -> m InstructionVector
parseAsVector = parseTLAsVector . tokenize

parseTLAsVector :: MonadSafeError m => TokenList -> m InstructionVector
parseTLAsVector tl = fromList <$> parseTL tl

liftedParseTL :: MonadSafeError m => TokenList -> m InstructionList
liftedParseTL = liftSafe . parseTL

parseTL :: MonadSafeError m => TokenList -> m InstructionList
parseTL = liftSafe . go where
  go :: TokenList -> Safe InstructionList
  go (T.MoveR   : tl) = (I.MoveR  :  ) <$> go tl
  go (T.MoveL   : tl) = (I.MoveL  :  ) <$> go tl
  go (T.Inc     : tl) = (I.Inc    :  ) <$> go tl
  go (T.Dec     : tl) = (I.Dec    :  ) <$> go tl
  go (T.Output  : tl) = (I.Output :  ) <$> go tl
  go (T.Input   : tl) = (I.Input  :  ) <$> go tl
  go []               = pure []
  go (T.JmpBack : tl) = liftErrorTuple ("JmpBack" , show tl)
  go (T.JmpPast : tl) = addWhile =<< parseWhile tl where
    addWhile (i , tl') = (i : ) <$> go tl'

parseWhile :: OperandParser Instruction
parseWhile tl = buildWhile <$> parseWhileD tl where
  buildWhile :: (InstructionDList , TokenList) -> (Instruction , TokenList)
  buildWhile (idl , tl') = (buildWhileFromDList idl , tl')

buildWhileFromDList :: InstructionDList -> Instruction
buildWhileFromDList = I.While . convert

parseWhileD :: OperandParser InstructionDList
parseWhileD = go D.empty where
  go :: InstructionDList -> TokenList -> Safe (InstructionDList , TokenList)
  go acc (T.MoveR   : tl) = go (acc `snoc` I.MoveR  ) tl
  go acc (T.MoveL   : tl) = go (acc `snoc` I.MoveL  ) tl
  go acc (T.Inc     : tl) = go (acc `snoc` I.Inc    ) tl
  go acc (T.Dec     : tl) = go (acc `snoc` I.Dec    ) tl
  go acc (T.Output  : tl) = go (acc `snoc` I.Output ) tl
  go acc (T.Input   : tl) = go (acc `snoc` I.Input  ) tl
  go acc              []  = liftErrorTuple ("End of List" , show acc)
  go acc (T.JmpBack : tl) = pure (acc , tl)
  go acc (T.JmpPast : tl) = snocInstruction =<< parseWhile tl where
    snocInstruction :: (Instruction , TokenList) -> Safe (InstructionDList , TokenList)
    snocInstruction (i , tl') = go (acc `snoc` i) tl'
