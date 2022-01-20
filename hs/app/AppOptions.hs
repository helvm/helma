{-# LANGUAGE StrictData #-}
module AppOptions where

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show langs)
                   <> value (show Cat)
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
                   <> help    "Compiler tokens, only for BF and ETA"
                   <> showDefault
                   )
  <*> switch       (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> strOption    (  long    "RAMType"
                   <> short   'm'
                   <> metavar "[RAMType]"
                   <> help   ("Implementation of RAM " <> show ramTypes)
                   <> value (show defaultRAMType)
                   <> showDefault
                   )
  <*> strOption    (  long    "StackType"
                   <> short   's'
                   <> metavar "[StackType]"
                   <> help   ("Implementation of Stack " <> show stackTypes)
                   <> value (show defaultStackType)
                   <> showDefault
                   )
  <*> strOption    (  long    "CellType"
                   <> short   'c'
                   <> metavar "[CellType]"
                   <> help   ("Implementation of Cell " <> show cellTypes)
                   <> value (show defaultCellType)
                   <> showDefault
                   )
  <*> strOption    (  long    "IntCellType"
                   <> short   'i'
                   <> metavar "[IntCellType]"
                   <> help   ("Implementation of IntCell " <> show intCellTypes)
                   <> value (show defaultIntCellType)
                   <> showDefault
                   )
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang        :: !String      -- | Lang
  , minified    :: !Minified
  , emitTL      :: !EmitTL
  , emitIL      :: !EmitIL
  , printLogs   :: !PrintLogs
  , compile     :: !Compile
  , asciiLabels :: !AsciiLabels
  , ramType     :: !String      -- | RAMType
  , stackType   :: !String      -- | StackType
  , cellType    :: !String      -- | CellType
  , intCellType :: !String      -- | IntCellType
  , exec        :: !Exec
  , file        :: !String
  }

type Minified    = Bool
type EmitIL      = Bool
type EmitTL      = Bool
type PrintLogs   = Bool
type Compile     = Bool
type AsciiLabels = Bool
type Exec        = Bool

----

data Lang = Cat | Rev | BF | ETA | SQ | STN | WS
  deriving stock (Eq , Read , Show)

langs :: [Lang]
langs = [Cat , Rev , BF , ETA , SQ , STN , WS]

parseLang :: String -> Lang
parseLang raw = (valid . readMaybe) raw where
  valid (Just a) = a
  valid Nothing  = error $ "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs
