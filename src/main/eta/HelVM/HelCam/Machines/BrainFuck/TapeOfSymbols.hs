module HelVM.HelCam.Machines.BrainFuck.TapeOfSymbols (FullTape, newTape, moveHeadRight, moveHeadLeft, wSuccSymbol, wPredSymbol, writeSymbol) where

import HelVM.HelCam.Machines.BrainFuck.Symbol

import HelVM.HelCam.Common.Tape
import HelVM.HelCam.Common.Util

----

newTape :: (Symbol s) => FullTape s
newTape = ([blank], [blank])

moveHeadRight :: (Symbol s) => FullTapeD s
moveHeadRight (cell:left, right) = pad (left, cell:right)
moveHeadRight ([], _)            = error "End of the Tipe"

moveHeadLeft :: (Symbol s) => FullTapeD s
moveHeadLeft (left, cell:right) = pad (cell:left, right)
moveHeadLeft (_, [])            = error "End of the Tipe"

pad :: (Symbol s) => FullTapeD s
pad ([], [])    = newTape
pad ([], right) = ([blank], right)
pad (left, [])  = (left, [blank])
pad tape        = tape

----

wSuccSymbol :: (Symbol s) => FullTapeD s
wSuccSymbol = modifyCell succSymbol

wPredSymbol :: (Symbol s) => FullTapeD s
wPredSymbol = modifyCell predSymbol

writeSymbol :: (Symbol s) => Char -> FullTapeD s
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: (Symbol s) => D s -> FullTapeD s
modifyCell modify (left, cell:right) = (left, modify cell:right)
modifyCell _ (_, [])                 = error "End of the Tipe"
