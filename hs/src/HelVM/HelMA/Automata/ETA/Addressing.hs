module HelVM.HelMA.Automata.ETA.Addressing (
  genericFindAddress,
  findAddress,
  genericNextLabel,
  nextLabel
) where

import HelVM.HelMA.Automata.ETA.Symbol
import HelVM.HelMA.Automata.ETA.Token

import HelVM.Common.Containers.Lookup
import HelVM.Common.Safe

import qualified Data.List as List

----

genericFindAddress :: Integral cell => TokenList -> cell -> Safe InstructionAddress
genericFindAddress il address = findAddress il $ fromIntegral address

findAddress :: TokenList -> Int -> Safe InstructionAddress
findAddress _  1 = pure 0
findAddress il address = (+1) <$> indexSafe (List.elemIndices R (il <> [R])) (address-2)

----

genericNextLabel :: Integral cell => TokenList -> InstructionAddress -> cell
genericNextLabel il ic = fromIntegral $ nextLabel il ic

nextLabel :: TokenList -> InstructionAddress -> Int
nextLabel il ic = length (List.elemIndices R il') + 2  where (il' , _) = splitAt ic il
