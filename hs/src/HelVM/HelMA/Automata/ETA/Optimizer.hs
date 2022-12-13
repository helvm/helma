module HelVM.HelMA.Automata.ETA.Optimizer where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import           Data.List.Extra
import qualified Data.List.Index                         as List

optimize :: MonadSafe m => TokenList -> m InstructionList
optimize = addEnd <.> join <.> optimizeLines

addEnd :: InstructionList -> InstructionList
addEnd l = l <> one End

optimizeLines :: MonadSafe m => TokenList -> m [InstructionList]
optimizeLines = sequence . uncurry optimizeLineWithIndex <.> splitOnRAndIndex

splitOnRAndIndex :: TokenList -> [(Int, TokenList)]
splitOnRAndIndex = List.indexed . splitOn [R]

optimizeLineWithIndex :: MonadSafe m => Int -> TokenList -> m InstructionList
optimizeLineWithIndex index = (dMarkI (fromIntegral $ index + 1) : ) <.> optimizeLine where
  optimizeLine :: MonadSafe m => TokenList -> m InstructionList

  optimizeLine (O : tl) = (sOutputI  : ) <$> optimizeLine tl
  optimizeLine (I : tl) = (sInputI   : ) <$> optimizeLine tl

  optimizeLine (S : tl) = (subI      : ) <$> optimizeLine tl
  optimizeLine (E : tl) = (divModI   : ) <$> optimizeLine tl

  optimizeLine (T : tl) = (Transfer : ) <$> optimizeLine tl
  optimizeLine (A : tl) = ((consI $ fromIntegral $ index + 2) : ) <$> optimizeLine tl

  optimizeLine (H : tl) = (halibutI : ) <$> optimizeLine tl
  optimizeLine (N : tl) = build =<< parseNumberFromTL tl where
    build :: MonadSafe m => (Integer , TokenList) -> m InstructionList
    build (n , tl') = (consI n :) <$> optimizeLine tl'

  optimizeLine (R : tl) = optimizeLine tl
  optimizeLine [] = pure []

consM :: Functor f => a -> f [a] -> f [a]
consM a l = (a : ) <$> l
