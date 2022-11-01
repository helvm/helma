module HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols (
  setSymbol,
  incSymbol,
  nextSymbol,
  prevSymbol,
  clearSymbol,
  writeSymbol,

  moveHead,
  moveHeadRight,
  moveHeadLeft,

  newTape,
  FullTape,
) where

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol

import           Control.Monad.Extra

-- | Change symbols

setSymbol :: (Symbol e) => Int -> FullTapeD e
setSymbol i = incSymbol i . clearSymbol

incSymbol :: (Symbol e) => Int -> FullTapeD e
incSymbol i = modifyCell (inc i)

clearSymbol :: (Symbol e) => FullTapeD e
clearSymbol = modifyCell $ const def

nextSymbol :: (Symbol e) => FullTapeD e
nextSymbol = modifyCell next

prevSymbol :: (Symbol e) => FullTapeD e
prevSymbol = modifyCell prev

writeSymbol :: (Symbol e) => Char -> FullTapeD e
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: D e -> FullTapeD e
modifyCell f (left , cell : right) = (left , f cell:right)
modifyCell _ (_ , [])              = error "End of the Tape"

-- | Moves

moveHead :: (Symbol e) => Int -> FullTapeD e
moveHead = changeTape moveHeadRight moveHeadLeft

changeTape :: FullTapeD e -> FullTapeD e -> Int -> FullTapeD e
changeTape lf gf i t = loop atc (i , t) where
  atc (i' , t') = (check . compare0) i' where
    check LT = Left (i' - 1 , lf t')
    check GT = Left (i' + 1 , gf t')
    check EQ = Right t'

moveHeadRight :: (Symbol e) => FullTapeD e
moveHeadRight (cell : left , right) = pad (left , cell : right)
moveHeadRight ([] , _)              = error "End of the Tape"

moveHeadLeft :: (Symbol e) => FullTapeD e
moveHeadLeft (left , cell : right) = pad (cell : left , right)
moveHeadLeft (_ , [])              = error "End of the Tape"

pad :: (Symbol e) => FullTapeD e
pad ([] , [])    = newTape
pad ([] , right) = ([def] , right)
pad (left , [])  = (left , [def])
pad tape         = tape

-- | Constructors

newTape :: (Symbol e) => FullTape e
newTape = ([def] , [def])

-- | Types

type D a = a -> a
type FullTape e = (HalfTape e , HalfTape e)
type FullTapeD e = D (FullTape e)

type HalfTape e = [e]
