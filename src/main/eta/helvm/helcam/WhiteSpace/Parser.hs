module HelVM.HelCam.WhiteSpace.Parser where

import HelVM.HelCam.WhiteSpace.Token
import HelVM.HelCam.WhiteSpace.Instruction

import Numeric.Natural

parse :: TokenList -> InstructionList
parse []           = []
-- Stack instructions
parse (S:S:xs)     = (Const  integer)    :(parse xs') where (integer, xs') = parseInteger xs
parse (S:T:S:_)    = panic "STS"
parse (S:T:T:xs)   = (Ref   int)         :(parse xs') where (int, xs') = parseInt xs
parse (S:T:N:xs)   = (Slide int)         :(parse xs') where (int, xs') = parseInt xs
parse (S:N:S:xs)   = Dup                 :(parse xs)
parse (S:N:T:xs)   = Swap                :(parse xs)
parse (S:N:N:xs)   = Discard             :(parse xs)
--Arithmetic
parse (T:S:S:S:xs) = (Binary Add)        :(parse xs)
parse (T:S:S:T:xs) = (Binary Sub)        :(parse xs)
parse (T:S:S:N:xs) = (Binary Mul)        :(parse xs)
parse (T:S:T:S:xs) = (Binary Div)        :(parse xs)
parse (T:S:T:T:xs) = (Binary Mod)        :(parse xs)
parse (T:S:T:N:_)  = panic "TSTN"
parse (T:S:N:S:_)  = panic "TSNS"
parse (T:S:N:T:_)  = panic "TSNT"
parse (T:S:N:N:_)  = panic "TSNN"
-- Heap access
parse (T:T:S:xs)   = Store               :(parse xs)
parse (T:T:T:xs)   = Load            :(parse xs)
parse (T:T:N:_)    = panic "TTN"
-- Control
parse (N:S:S:xs)   = (Label       label) :(parse xs') where (label, xs') = parseNatural xs
parse (N:S:T:xs)   = (Call        label) :(parse xs') where (label, xs') = parseNatural xs
parse (N:S:N:xs)   = (Jump        label) :(parse xs') where (label, xs') = parseNatural xs
parse (N:T:S:xs)   = (Branch EZ   label) :(parse xs') where (label, xs') = parseNatural xs
parse (N:T:T:xs)   = (Branch Neg  label) :(parse xs') where (label, xs') = parseNatural xs
parse (N:T:N:xs)   = Return              :(parse xs)
parse (N:N:S:_)    = panic "NNS"
parse (N:N:T:_)    = panic "NNT"
parse (N:N:N:xs)   = End                 :(parse xs)
-- IO instructions
parse (T:N:S:S:xs) = OutputChar          :(parse xs)
parse (T:N:S:T:xs) = OutputNum           :(parse xs)
parse (T:N:S:N:_)  = panic "TNSN"
parse (T:N:T:S:xs) = InputChar           :(parse xs)
parse (T:N:T:T:xs) = InputNum            :(parse xs)
parse (T:N:T:N:_)  = panic "TNTN"
parse (T:N:N:S:_)  = panic "TNNS"
parse (T:N:N:T:_)  = panic "TNNT"
parse (T:N:N:N:_)  = panic "TNNN"
parse tokens       = panic $ show tokens

panic :: String -> InstructionList
panic token = error $ "Unrecognised " ++ token

parseInt :: TokenList -> (Int, TokenList)
parseInt xs = (fromIntegral (integer), xs') where (integer, xs') = parseInteger xs

parseInteger :: TokenList -> (Integer, TokenList)
parseInteger (S:xs) = parseIntegral' xs []
parseInteger (T:xs) = revertIntegral $ parseIntegral' xs []
parseInteger (N:xs) = (0,xs)
parseInteger []     = error "EOL"

revertIntegral :: (Integer, TokenList) -> (Integer, TokenList)
revertIntegral (i,l) = (-i,l)

parseNatural :: TokenList -> (Natural, TokenList)
parseNatural xs = parseIntegral' xs []

parseIntegral' :: (Integral a) =>  TokenList -> TokenList -> (a, TokenList)
parseIntegral' (N:xs) acc = (makeIntegral acc, xs)
parseIntegral' (x:xs) acc = parseIntegral' xs (x:acc)
parseIntegral' []     acc = error $ show acc

makeIntegral :: (Integral a) => TokenList -> a
makeIntegral = foldr (shiftAndAdd) 0 . map toBit

shiftAndAdd :: (Integral a) => a -> a -> a
shiftAndAdd bit acc = acc * 2 + bit
