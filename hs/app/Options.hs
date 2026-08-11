module Options where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Lang
import           HelVM.HelMA.Automaton.API.LogLevel
import           HelVM.HelMA.Automaton.API.MemoryOptions
import           HelVM.HelMA.Automaton.API.OptimizationLevel

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.LabelType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType
import           HelVM.HelMA.Automata.ETA.API.AutomatonType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType


import           Data.MonoTraversable

import           Options.Applicative

optionsParser ∷ Parser AppOptions
optionsParser = AppOptions
  <$> logLevelParser
  <*> option auto  (  long    "Emit"
                   <> short   'E'
                   <> metavar "[Emit]"
                   <> help   ("Type of emit " <> show emits)
                   <> value    defaultEmit
                   <> showDefault
                   )
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> flag BinaryLabel TextLabel
                   (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> memoryOptionsParser
  <*> autoOptionsParser
  <*> langCommandParser
  <*> argument str (  metavar "FILE")

logLevelParser ∷ Parser LogLevel
logLevelParser = explicitVerbosity <|> countedVerbosity <|> pure defaultLogLevel where
  explicitVerbosity = option auto
    (  long    "verbosity"
    <> metavar "[LogLevel]"
    <> help   ("Verbosity level " <> show logLevels)
    )
  countedVerbosity = logLevelFromCount . olength <$> many parseV where
    parseV = flag' ()
      (  short   'v'
      <> help    "Increase verbosity level (can be repeated, e.g. -vvv)"
      )

memoryOptionsParser ∷ Parser MemoryOptions
memoryOptionsParser = MemoryOptions
  <$> option auto ( long "RAMType"
                 <> short 'm'
                 <> metavar "[RAMType]"
                 <> help ("Implementation of RAM " <> show ramTypes)
                 <> value defaultRAMType
                 <> showDefault )
  <*> option auto ( long "StackType"
                 <> short 's'
                 <> metavar "[StackType]"
                 <> help ("Implementation of Stack " <> show stackTypes)
                 <> value defaultStackType
                 <> showDefault )
  <*> option auto ( long "CellType"
                 <> short 'c'
                 <> metavar "[CellType]"
                 <> help ("Implementation of Cell " <> show cellTypes)
                 <> value defaultCellType
                 <> showDefault )
  <*> option auto ( long "IntCellType"
                 <> short 'i'
                 <> metavar "[IntCellType]"
                 <> help ("Implementation of IntCell " <> show intCellTypes)
                 <> value defaultIntCellType
                 <> showDefault )

autoOptionsParser ∷ Parser AutoOptions
autoOptionsParser = AutoOptions
  <$> optLevelParser
  <*> option auto  (  long    "Limit"
                   <> short   'L'
                   <> metavar "[Limit]"
                   <> value    Nothing
                   <> showDefault
                   )
  <*> option auto  (  long    "DumpType"
                   <> short   'd'
                   <> metavar "[DumpType]"
                   <> help   ("Implementation of DumpType " <> show dumpTypes)
                   <> value    defaultDumpType
                   <> showDefault
                   )

optLevelParser ∷ Parser OptimizationLevel
optLevelParser = explicitOpt <|> countedOpt <|> pure defaultOptimizationLevel where
  explicitOpt = fromNatural <$> option auto
    (  long    "optimize"
    <> short   'O'
    <> metavar "[Natural]"
    <> help   ("Optimization level " <> show optimizationLevels)
    )
  countedOpt = fromInt . olength <$> many parseO where
    parseO = flag' ()
      (  short   'O'
      <> help    "Increase optimization level (can be repeated, e.g. -OOOO)"
      )

langCommandParser ∷ Parser LangCommand
langCommandParser = subparser
  (  command "bf"   (info bfParser           (progDesc "BrainFuck interpreter"))
  <> command "eta"  (info etaParser          (progDesc "ETA interpreter"))
  <> command "f"    (info (pure FCommand)    (progDesc "F_ interpreter"))
  <> command "piet" (info pietParser         (progDesc "Piet interpreter"))
  <> command "sq"   (info (pure SQCommand)   (progDesc "SQ interpreter"))
  <> command "ws"   (info wsParser           (progDesc "WhiteSpace interpreter"))
  <> command "cat"  (info (pure CatCommand)  (progDesc "Cat interpreter"))
  <> command "rev"  (info (pure RevCommand)  (progDesc "Rev interpreter"))
  <> command "lazy" (info (pure LazyCommand) (progDesc "Lazy interpreter"))
  <> command "zot"  (info (pure ZotCommand)  (progDesc "Zot interpreter"))
  ) where
    bfParser = BFCommand
      <$> option auto (long "ImplType" <> short 'b' <> metavar "[ImplType]" <> value defaultImplType <> showDefault)

    etaParser = ETACommand
      <$> option auto (long "AutomatonType" <> short 'i' <> metavar "[AutomatonType]" <> value defaultAutomatonType <> showDefault)

    pietParser = PietCommand
      <$> optional (option auto (long "LexerType" <> short 'l' <> metavar "[LexerType]" <> value defaultLexerType <> showDefault))
      <*> optional (option auto (long "codels" <> short 'C' <> metavar "[LENGTH]" <> help "codel length (the codel size will be LENGTH^2)" ))

    wsParser = WSCommand
      <$> flag WhiteTokenType VisibleTokenType (long "tokenType" <> short 't' <> showDefault)
