{-# LANGUAGE StrictData #-}
module AppOptions where

import           BoolTypes
import           Emit
import qualified Lang

import qualified HelVM.HelMA.Automaton.API.AutoOptions       as API
import qualified HelVM.HelMA.Automaton.API.EvalParams        as API
import           HelVM.HelMA.Automaton.API.IOTypes           as API
import qualified HelVM.HelMA.Automaton.API.MemoryOptions     as API
import           HelVM.HelMA.Automaton.API.OptimizationLevel as API

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

optionParser :: Parser AppOptions
optionParser = AppOptions
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
                   <> help   ("Language to interpret " <> show Lang.langs)
                   <> value    Lang.defaultLang
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

-- | Methods

langWithOptions :: AppOptions -> Lang.LangWithOptions
langWithOptions o = Lang.LangWithOptions (lang o) (bfType o) (etaImplType o) (tokenType o)

evalParams :: AppOptions -> Source -> API.EvalParams
evalParams o source = API.EvalParams (formatType o) source (memoryOptions o) (autoOptions o)

memoryOptions :: AppOptions -> API.MemoryOptions
memoryOptions o = API.MemoryOptions (ramType o) (stackType o) (cellType o) (intCellType o)

autoOptions :: AppOptions -> API.AutoOptions
autoOptions o = API.AutoOptions (API.fromBool $ optimizationFlag o) Nothing (dumpType o)

isImage :: AppOptions -> Bool
isImage = piet

-- | Types

data AppOptions = AppOptions
  { emit             :: !Emit
  , printLogs        :: !PrintLogs

  , lang             :: !Lang.Lang
  , bfType           :: !BFType
  , etaImplType      :: !ETAImplType
  , tokenType        :: !TokenType

  , optimizationFlag :: !Optimization
  , formatType       :: !FormatType
  , ramType          :: !RAMType
  , stackType        :: !StackType
  , cellType         :: !CellType
  , intCellType      :: !IntCellType
  , dumpType         :: !DumpType
  , exec             :: !Exec
  , piet             :: !Piet
  , file             :: !String
  }
