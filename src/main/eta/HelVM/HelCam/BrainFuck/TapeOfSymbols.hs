module HelVM.HelCam.BrainFuck.TapeOfSymbols (Tape, newTape, moveHeadRight, moveHeadLeft, wSuccSymbol, wPredSymbol, writeSymbol) where

import HelVM.HelCam.BrainFuck.Symbol

import HelVM.HelCam.Common.Util

type HalfTape a = [a]
type Tape a = (HalfTape a, HalfTape a)
type TapeD a = Tape a -> Tape a

----

newTape :: (Symbol s) => Tape s
newTape = ([blank], [blank])

moveHeadRight :: (Symbol s) => TapeD s
moveHeadRight (cell:left, right) = pad (left, cell:right)
moveHeadRight ([], _)            = error "End of the Tipe"

moveHeadLeft :: (Symbol s) => TapeD s
moveHeadLeft (left, cell:right) = pad (cell:left, right)
moveHeadLeft (_, [])            = error "End of the Tipe"

pad :: (Symbol s) => TapeD s
pad ([], [])    = newTape
pad ([], right) = ([blank], right)
pad (left, [])  = (left, [blank])
pad tape        = tape

----

wSuccSymbol :: (Symbol s) => TapeD s
wSuccSymbol = modifyCell succSymbol

wPredSymbol :: (Symbol s) => TapeD s
wPredSymbol = modifyCell predSymbol

writeSymbol :: (Symbol s) => Char -> TapeD s
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: (Symbol s) => D s -> TapeD s
modifyCell modify (left, cell:right) = (left, modify cell:right)
modifyCell _ (_, [])                 = error "End of the Tipe"
