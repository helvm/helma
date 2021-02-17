module HelVM.HelCam.Machines.BrainFuck.TapeOfSymbols (
  FullTape,
  newTape,
  moveHeadRight,
  moveHeadLeft,
  wNextSymbol,
  wPrevSymbol,
  writeSymbol
) where

import HelVM.HelCam.Machines.BrainFuck.Symbol

import HelVM.HelCam.Common.Util

----

type FullTape s = (HalfTape s, HalfTape s)
type FullTapeD s = D (FullTape s)

type HalfTape s = [s]

----

newTape :: (Symbol s) => FullTape s
newTape = ([def], [def])

moveHeadRight :: (Symbol s) => FullTapeD s
moveHeadRight (cell:left, right) = pad (left, cell:right)
moveHeadRight ([], _)            = error "End of the Tipe"

moveHeadLeft :: (Symbol s) => FullTapeD s
moveHeadLeft (left, cell:right) = pad (cell:left, right)
moveHeadLeft (_, [])            = error "End of the Tipe"

pad :: (Symbol s) => FullTapeD s
pad ([], [])    = newTape
pad ([], right) = ([def], right)
pad (left, [])  = (left, [def])
pad tape        = tape

----

wNextSymbol :: (Symbol s) => FullTapeD s
wNextSymbol = modifyCell next

wPrevSymbol :: (Symbol s) => FullTapeD s
wPrevSymbol = modifyCell prev

writeSymbol :: (Symbol s) => Char -> FullTapeD s
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: (Symbol s) => D s -> FullTapeD s
modifyCell f (left, cell:right) = (left, f cell:right)
modifyCell _ (_, [])            = error "End of the Tape"
