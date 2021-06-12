module HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols (
  FullTape,
  newTape,
  moveHeadRight,
  moveHeadLeft,
  wNextSymbol,
  wPrevSymbol,
  writeSymbol
) where

import HelVM.HelMA.Automata.BrainFuck.Symbol

import HelVM.Common.Util

----

type FullTape e = (HalfTape e , HalfTape e)
type FullTapeD e = D (FullTape e)

type HalfTape e = [e]

----

newTape :: (Symbol e) => FullTape e
newTape = ([def] , [def])

moveHeadRight :: (Symbol e) => FullTapeD e
moveHeadRight (cell:left , right) = pad (left , cell:right)
moveHeadRight ([] , _)            = error "End of the Tipe"

moveHeadLeft :: (Symbol e) => FullTapeD e
moveHeadLeft (left , cell:right) = pad (cell:left , right)
moveHeadLeft (_ , [])            = error "End of the Tipe"

pad :: (Symbol e) => FullTapeD e
pad ([] , [])    = newTape
pad ([] , right) = ([def] , right)
pad (left , [])  = (left , [def])
pad tape        = tape

----

wNextSymbol :: (Symbol e) => FullTapeD e
wNextSymbol = modifyCell next

wPrevSymbol :: (Symbol e) => FullTapeD e
wPrevSymbol = modifyCell prev

writeSymbol :: (Symbol e) => Char -> FullTapeD e
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: D e -> FullTapeD e
modifyCell f (left , cell:right) = (left , f cell:right)
modifyCell _ (_ , [])            = error "End of the Tape"
