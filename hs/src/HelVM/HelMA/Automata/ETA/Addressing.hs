module HelVM.HelMA.Automata.ETA.Addressing (
  genericFindAddress,
  findAddress,
  genericNextLabel,
  nextLabel
) where

import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Common.Containers.LLIndexSafe

import           HelVM.Common.Control.Safe

import           Data.ListLike

import           Prelude                             hiding (length, splitAt)

import qualified Data.Vector                         as Vector

----

genericFindAddress :: Integral cell => Vector.Vector Token -> cell -> Safe InstructionAddress
genericFindAddress il = findAddress il . fromIntegral

findAddress :: Vector.Vector Token -> Int -> Safe InstructionAddress
findAddress _  1       = pure 0
findAddress il address = (+1) <$> indexSafe (Vector.elemIndices R il) (address-2)

----

genericNextLabel :: Integral cell => Vector.Vector Token -> InstructionAddress -> cell
genericNextLabel il = fromIntegral . nextLabel il

nextLabel :: Vector.Vector Token -> InstructionAddress -> Int
nextLabel il ic = length (Vector.elemIndices R il') + 2  where (il' , _) = splitAt ic il
