{-# LANGUAGE StrictData #-}
module Options where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Lang
import           HelVM.HelMA.Automaton.API.LogLevel

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.LabelType
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
  <*> option auto  (  long    "verbosity"
                   <> short   'v'
                   <> metavar "[LogLevel]"
                   <> help   ("Verbosity level " <> show logLevels)
                   <> value    defaultLogLevel
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
  <*> optional (option auto
      (  long "codels"
      <> short 'c'
      <> metavar "LENGTH"
      <> help "codel length (the codel size will be LENGTH^2)" ))
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> langCommandParser
  <*> argument str (  metavar "FILE")

langCommandParser :: Parser LangCommand
langCommandParser = subparser
  (  command "bf"   (info bfParser   (progDesc "BrainFuck interpreter"))
  <> command "eta"  (info etaParser  (progDesc "ETA interpreter"))
  <> command "f"    (info (pure FCommand) (progDesc "F_ interpreter"))
  <> command "piet" (info (pure PietCommand) (progDesc "Piet interpreter"))
  <> command "sq"   (info (pure SQCommand) (progDesc "SQ interpreter"))
  <> command "ws"   (info wsParser   (progDesc "WhiteSpace interpreter"))
  <> command "cat"  (info (pure CatCommand) (progDesc "Cat interpreter"))
  <> command "rev"  (info (pure RevCommand) (progDesc "Rev interpreter"))
  <> command "lazy" (info (pure LazyCommand) (progDesc "Lazy interpreter"))
  <> command "zot"  (info (pure ZotCommand) (progDesc "Zot interpreter"))
  ) where
    bfParser = BFCommand
      <$> option auto (long "BFType" <> short 'b' <> metavar "[BFType]" <> value defaultBFType <> showDefault)

    etaParser = ETACommand
      <$> option auto (long "ETAImplType" <> metavar "[ETAImplType]" <> value defaultETAImplType <> showDefault)

    wsParser = WSCommand
      <$> flag WhiteTokenType VisibleTokenType (long "tokenType" <> short 't' <> help "Visible tokens for WS")
