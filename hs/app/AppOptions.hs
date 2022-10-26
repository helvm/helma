{-# LANGUAGE StrictData #-}
module AppOptions where

import           Lang

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> option auto  (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show langs)
                   <> value    defaultLang
                   <> showDefault
                   )
  <*> flag WhiteTokenType VisibleTokenType
                   (  long    "visibleTokes"
                   <> short   't'
                   <> help    "Visible tokens for WS"
                   <> showDefault
                   )
  <*> switch       (  long    "minification"
                   <> short   'M'
                   <> help    "Emit minified code"
                   <> showDefault
                   )
  <*> switch       (  long    "emit-tl"
                   <> short   'T'
                   <> help    "Emit the lexed tokens"
                   <> showDefault
                   )
  <*> switch       (  long    "emit-il"
                   <> short   'I'
                   <> help    "Emit the parsed instructions"
                   <> showDefault
                   )
  <*> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
                   <> showDefault
                   )
  <*> switch       (  long    "compile"
                   <> short   'C'
                   <> help    "Compiler tokens, only for ETA"
                   <> showDefault
                   )
  <*> flag BinaryLabel TextLabel
                   (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> option auto  (  long    "BFType"
                   <> short   'b'
                   <> metavar "[BFType]"
                   <> help   ("Type of BF implementation " <> show bfTypes)
                   <> value    defaultBFType
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
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang          :: !Lang
  , visibleTokens :: !TokenType
  , minified      :: !Minified
  , emitTL        :: !EmitTL
  , emitIL        :: !EmitIL
  , printLogs     :: !PrintLogs
  , compile       :: !Compile
  , formatType    :: !FormatType
  , bfType        :: !BFType
  , ramType       :: !RAMType
  , stackType     :: !StackType
  , cellType      :: !CellType
  , intCellType   :: !IntCellType
  , dumpType      :: !DumpType
  , exec          :: !Exec
  , file          :: !String
  }

type Minified      = Bool
type EmitIL        = Bool
type EmitTL        = Bool
type PrintLogs     = Bool
type Compile       = Bool
type Exec          = Bool
