{-# LANGUAGE StrictData #-}
module Options where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Lang

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType
import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           Options.Applicative

optionsParser :: Parser AppOptions
optionsParser = AppOptions
  <$> option auto  (  long    "Emit"
                   <> short   'E'
                   <> metavar "[Emit]"
                   <> help   ("Type of emit " <> show emits)
                   <> value    defaultEmit
                   <> showDefault
                   )
  <*> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
                   <> showDefault
                   )
  <*> option auto  (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show langs)
                   <> value    defaultLang
                   <> showDefault
                   )
  <*> option auto  (  long    "BFType"
                   <> short   'b'
                   <> metavar "[BFType]"
                   <> help   ("Type of BF implementation " <> show bfTypes)
                   <> value    defaultBFType
                   <> showDefault
                   )
  <*> option auto  (  long    "ETAImplType"
                   <> metavar "[ETAImplType]"
                   <> help   ("Type of ETA implementation " <> show etaImplTypes)
                   <> value    defaultETAImplType
                   <> showDefault
                   )
  <*> flag WhiteTokenType VisibleTokenType
                   (  long    "tokenType"
                   <> short   't'
                   <> help    "Visible tokens for WS"
                   <> showDefault
                   )
  <*> switch       (  long    "optimize"
                   <> short   'O'
                   <> help    "Optimize instructions"
                   <> showDefault
                   )
  <*> flag BinaryLabel TextLabel
                   (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> option auto  (  long    "RAMType"
                   <> short   'm'
                   <> metavar "[RAMType]"
                   <> help   ("Implementation of RAM " <> show ramTypes)
                   <> value    defaultRAMType
                   <> showDefault
                   )
  <*> option auto  (  long    "StackType"
                   <> short   's'
                   <> metavar "[StackType]"
                   <> help   ("Implementation of Stack " <> show stackTypes)
                   <> value    defaultStackType
                   <> showDefault
                   )
  <*> option auto  (  long    "CellType"
                   <> short   'c'
                   <> metavar "[CellType]"
                   <> help   ("Implementation of Cell " <> show cellTypes)
                   <> value    defaultCellType
                   <> showDefault
                   )
  <*> option auto  (  long    "IntCellType"
                   <> short   'i'
                   <> metavar "[IntCellType]"
                   <> help   ("Implementation of IntCell " <> show intCellTypes)
                   <> value    defaultIntCellType
                   <> showDefault
                   )
  <*> option auto  (  long    "DumpType"
                   <> short   'd'
                   <> metavar "[DumpType]"
                   <> help   ("Implementation of DumpType " <> show dumpTypes)
                   <> value    defaultDumpType
                   <> showDefault
                   )
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> switch       (  long    "piet"
                   <> short   'p'
                   <> help    "Piet"
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")
