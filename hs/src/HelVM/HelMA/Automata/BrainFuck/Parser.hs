module HelVM.HelMA.Automata.BrainFuck.Parser where

import           HelVM.HelMA.Automata.BrainFuck.Instruction.FlatInstruction as Flat
import           HelVM.HelMA.Automata.BrainFuck.Instruction.TreeInstruction as Tree
import           HelVM.HelMA.Automata.BrainFuck.Lexer

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ListLikeUtil

import           Data.ListLike                                              hiding (show)

import qualified Data.DList                                                 as D

type OperandParser m a = FlatTreeInstructionList -> m (a , FlatTreeInstructionList)

parseAsVector :: MonadSafe m => Source -> m TreeInstructionVector
parseAsVector = parseFILAsVector . tokenize

parseFILAsVector :: MonadSafe m => FlatTreeInstructionList -> m TreeInstructionVector
parseFILAsVector fil = fromList <$> parseFIL fil

parseFIL :: MonadSafe m => FlatTreeInstructionList -> m TreeInstructionList
parseFIL (Flat.Simple i : fil) = (Tree.Simple i :  ) <$> parseFIL fil
parseFIL []                = pure []
parseFIL (Flat.JmpBack  : fil) = liftErrorWithPrefix "JmpBack" $ show fil
parseFIL (Flat.JmpPast  : fil) = addWhile =<< parseWhile fil where
  addWhile (i , fil') = (i : ) <$> parseFIL fil'

parseWhile :: MonadSafe m => OperandParser m TreeInstruction
parseWhile fil = buildWhile <$> parseWhileD fil where
  buildWhile :: (TreeInstructionDList , FlatTreeInstructionList) -> (TreeInstruction , FlatTreeInstructionList)
  buildWhile (idl , fil') = (buildWhileFromDList idl , fil')

buildWhileFromDList :: TreeInstructionDList -> TreeInstruction
buildWhileFromDList = Tree.While . convert

parseWhileD :: MonadSafe m => OperandParser m TreeInstructionDList
parseWhileD = go D.empty where
  go :: MonadSafe m => TreeInstructionDList -> FlatTreeInstructionList -> m (TreeInstructionDList , FlatTreeInstructionList)
  go acc (Flat.Simple i : fil) = go (acc `snoc` Tree.Simple i  ) fil
  go acc                  []  = liftErrorWithPrefix "End of List" $ show acc
  go acc (Flat.JmpBack  : fil) = pure (acc , fil)
  go acc (Flat.JmpPast  : fil) = snocInstruction =<< parseWhile fil where
    snocInstruction :: MonadSafe m => (TreeInstruction , FlatTreeInstructionList) -> m (TreeInstructionDList , FlatTreeInstructionList)
    snocInstruction (i , fil') = go (acc `snoc` i) fil'
