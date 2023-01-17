module HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols (
  triAndClearSymbol,

  mulDupAndClearSymbol,
  dupAndClearSymbol,

  mulAddAndClearSymbol,
  addAndClearSymbol,
  subAndClearSymbol,

  setSymbol,
  incSymbol,
  nextSymbol,
  prevSymbol,
  clearSymbol,
  writeSymbol,

  moveHead,
  moveHeadRight,
  moveHeadLeft,

  readSymbol,
  readSymbolSafe,

  newTape,
  FullTape,
  FullTapeD,
) where

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe

import           Control.Monad.Extra

-- | Complex instructions

triAndClearSymbol :: (Symbol e) => Integer -> Integer -> Integer -> FullTapeD e
triAndClearSymbol f1 f2 f3 tape = tape & stepSymbol f1 & stepSymbol f2 & stepSymbol f3 & backAndClear back where
  back = negate (f1 + f2 + f3)
  stepSymbol = step symbol
  symbol = readSymbol tape

mulDupAndClearSymbol :: (Symbol e) => Integer -> Integer -> Integer -> Integer -> FullTapeD e
mulDupAndClearSymbol m1 m2 f1 f2 tape = tape & step ms1 f1 & step ms2 f2 & backAndClear back where
  back = negate (f1 + f2)
  ms1 = symbol * fromIntegral m1
  ms2 = symbol * fromIntegral m2
  symbol = readSymbol tape

dupAndClearSymbol :: (Symbol e) => Integer -> Integer -> FullTapeD e
dupAndClearSymbol f1 f2 tape = tape & stepSymbol f1 & stepSymbol f2 & backAndClear back where
  back = negate (f1 + f2)
  stepSymbol = step symbol
  symbol = readSymbol tape

mulAddAndClearSymbol :: (Symbol e) => Integer -> Integer -> FullTapeD e
mulAddAndClearSymbol mul forward tape = tape & step mulSymbol forward & backAndClear back where
  back = negate forward
  mulSymbol = symbol * fromIntegral mul
  symbol = readSymbol tape

addAndClearSymbol :: (Symbol e) => Integer -> FullTapeD e
addAndClearSymbol = changeAndClearSymbol id

subAndClearSymbol :: (Symbol e) => Integer -> FullTapeD e
subAndClearSymbol = changeAndClearSymbol negate

changeAndClearSymbol :: (Symbol e) => (e -> e) -> Integer -> FullTapeD e
changeAndClearSymbol f forward tape = tape & step symbol forward & backAndClear back where
  back = negate forward
  symbol = f $ readSymbol tape

step :: (Symbol e) => e -> Integer -> FullTapeD e
step symbol forward = addSymbol symbol . moveHead forward

backAndClear :: (Symbol e) => Integer -> FullTapeD e
backAndClear back = clearSymbol . moveHead back

-- | Change symbols

setSymbol :: (Symbol e) => Integer -> FullTapeD e
setSymbol i = modifyCell $ const $ fromIntegral i

incSymbol :: (Symbol e) => Integer -> FullTapeD e
incSymbol i = addSymbol $ fromIntegral i

addSymbol :: (Symbol e) => e -> FullTapeD e
addSymbol e = modifyCell $ inc e

clearSymbol :: (Symbol e) => FullTapeD e
clearSymbol = modifyCell $ const def

nextSymbol :: (Symbol e) => FullTapeD e
nextSymbol = modifyCell next

prevSymbol :: (Symbol e) => FullTapeD e
prevSymbol = modifyCell prev

writeSymbol :: (Symbol e) => Char -> FullTapeD e
writeSymbol symbol = modifyCell (const $ fromChar symbol)

modifyCell :: D e -> FullTapeD e
modifyCell f (left , cell : right) = (left , f cell : right)
modifyCell _ (_ , [])              = error "End of the Tape"

readSymbol :: FullTape e -> e
readSymbol (_ , cell : _) = cell
readSymbol (_ , [])       = error "End of the Tape"

readSymbolSafe :: MonadSafe m => FullTape e -> m e
readSymbolSafe = findSafe 0 . snd

-- | Moves

moveHead :: (Symbol e) => Integer -> FullTapeD e
moveHead = changeTape moveHeadRight moveHeadLeft

changeTape :: FullTapeD e -> FullTapeD e -> Integer -> FullTapeD e
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
