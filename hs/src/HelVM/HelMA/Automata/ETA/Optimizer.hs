module HelVM.HelMA.Automata.ETA.Optimizer (
  optimize,
)
where

import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import           Data.List.Extra
import qualified Data.List.Index                         as List

import qualified Data.ListLike                           as LL

optimize :: MonadSafe m => TokenList -> m InstructionList
optimize = appendEnd <.> join <.> optimizeLines

appendEnd :: InstructionList -> InstructionList
appendEnd l = l <> one End

optimizeLines :: MonadSafe m => TokenList -> m [InstructionList]
optimizeLines = sequence . optimizeLineInit <.> lineFromTuple2 <.> splitOnRAndIndex2

splitOnRAndIndex2 :: TokenList -> [(Natural, [TokenList])]
splitOnRAndIndex2 = indexedByNaturalWithOffset 1 <.> List.indexed . filterNull . tails . splitOn [R]

indexedByNaturalWithOffset :: Int -> (Int , a) -> (Natural , a)
indexedByNaturalWithOffset offset (i , a) = (fromIntegral (i + offset) , a)

optimizeLineInit :: MonadSafe m => Line -> m InstructionList
optimizeLineInit line = (dMarkI (currentAddress line) : ) <$> optimizeLineTail line

optimizeLineTail:: MonadSafe m => Line -> m InstructionList
optimizeLineTail line = check (currentTL line) where
  check (t : tl) = optimizeLineForToken t $ line { currentTL = tl }
  check []       = pure []

optimizeLineForToken :: MonadSafe m => Token -> Line -> m InstructionList
optimizeLineForToken O = (sOutputI  : ) <.> optimizeLineTail
optimizeLineForToken I = (sInputI   : ) <.> optimizeLineTail

optimizeLineForToken S = (subI      : ) <.> optimizeLineTail
optimizeLineForToken E = prependDivMod

optimizeLineForToken H = (halibutI  : ) <.> optimizeLineTail
optimizeLineForToken T = (Transfer  : ) <.> optimizeLineTail

optimizeLineForToken A = prependAddress
optimizeLineForToken N = prependNumber

optimizeLineForToken R = optimizeLineTail

prependDivMod :: MonadSafe m => Line -> m InstructionList
prependDivMod line = check $ numberFlag line where
  check False = prependDivModSimple line
  check True  = prependStaticMakr line <.> optimizeLineTail $ line {numberFlag = False}

prependStaticMakr :: Line -> InstructionList -> InstructionList
prependStaticMakr line il = divModI : sMarkIN (currentAddress line) : il

prependDivModSimple :: MonadSafe m => Line -> m InstructionList
prependDivModSimple = (divModI : ) <.> optimizeLineTail

prependAddress :: MonadSafe m => Line -> m InstructionList
prependAddress line = ((consI $ fromIntegral $ nextAddress line) : ) <$> optimizeLineTail line

prependNumber :: MonadSafe m => Line -> m InstructionList
prependNumber line = flip buildNumber line =<< parseNumberFromTLL (currentTL line , nextTLL line)

buildNumber :: MonadSafe m => (Integer , (TokenList , [TokenList])) -> Line -> m InstructionList
buildNumber (n , (tl , ttl) ) line = build (LL.length (nextTLL line) - LL.length ttl) where
  build 0      = (consI n :) <$> optimizeLineTail (line {currentTL = tl})
  build offset = pure [consI n , sJumpIN $ currentAddress line + fromIntegral offset]

-- | Accessors

nextAddress :: Line -> Natural
nextAddress line = currentAddress line + 1

-- | Constructors

lineFromTuple2 :: (Natural, [TokenList]) -> Line
lineFromTuple2 (a, []) = Line
  { currentAddress = a
  , currentTL = []
  , nextTLL = []
  , numberFlag = True
  }
lineFromTuple2 (a, l : ls) = Line
  { currentAddress = a
  , currentTL = l
  , nextTLL = ls
  , numberFlag = True
  }

data Line = Line
  { currentTL      :: TokenList
  , currentAddress :: Natural
  , numberFlag     :: Bool
  , nextTLL        :: [TokenList]
  }

--consM :: Functor f => a -> f [a] -> f [a]
--consM a l = (a : ) <$> l

filterNull :: [[a]] -> [[a]]
filterNull = filter notNull
