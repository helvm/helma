{-# Language DataKinds          #-}
{-# Language ExplicitNamespaces #-}

module AppOptions where

import HelVM.HelCam.Common.Types.CellType
import HelVM.HelCam.Common.Types.RAMType

import Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show langs)
                   <> value (show Cat)
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
  <*> switch       (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> strOption    (  long    "impl"
                   <> short   'i'
                   <> metavar "[IMPL]"
                   <> help   ("Implementation of interpreter " <> show impls)
                   <> value (show Monadic)
                   <> showDefault
                   )
  <*> strOption    (  long    "RAMType"
                   <> short   'm'
                   <> metavar "[RAMType]"
                   <> help   ("Implementation of RAM " <> show ramTypes)
                   <> value (show ListRAMType)
                   <> showDefault
                   )
  <*> strOption    (  long    "CellType"
                   <> short   'c'
                   <> metavar "[CellType]"
                   <> help   ("Implementation of Cell " <> show cellTypes)
                   <> value (show ListRAMType)
                   <> showDefault
                   )
  <*> switch       (  long    "exec"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")


data AppOptions = AppOptions
  { lang        :: String      -- Lang
  , emitTL      :: EmitTL
  , emitIL      :: EmitIL
  , asciiLabels :: AsciiLabels
  , impl        :: String      -- Impl
  , ramType     :: String      -- RAMType
  , cellType    :: String      -- CellType
  , exec        :: Exec
  , file        :: String
  }

type EmitIL      = Bool
type EmitTL      = Bool
type AsciiLabels = Bool
type EtaMode     = Bool
type Exec        = Bool

----

data Lang = Cat | BF | ETA | SQ | WS
  deriving (Eq, Read, Show)

langs :: [Lang]
langs = [Cat, BF, ETA, SQ, WS]

parseLang :: String -> Lang
parseLang raw = valid $ readMaybe raw where
  valid (Just a) = a
  valid Nothing  = error $ "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs

----

data Impl = Monadic | Interact deriving (Eq, Read, Show)

impls :: [Impl]
impls = [Monadic, Interact]

parseImpl :: String -> Impl
parseImpl raw = valid $ readMaybe raw where
  valid (Just a) = a
  valid Nothing  = error $ "Impl '" <> toText raw <> "' is not valid impl. Valid impls are : " <> show impls
