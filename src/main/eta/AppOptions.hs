{-# Language DataKinds          #-}
{-# Language ExplicitNamespaces #-}

module AppOptions where

import Options.Applicative

import Text.Read

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " ++ show langs)
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
  <*> switch       (  long    "eta"
                   <> short   'E'
                   <> help    "Eta compliance mode"
                   <> showDefault
                   )
  <*> strOption    (  long    "impl"
                   <> short   'i'
                   <> metavar "[IMPL]"
                   <> help   ("Implementation of interpreter " ++ show impls)
                   <> value (show Monadic)
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
  , etaMode     :: EtaMode
  , impl        :: String      -- Impl
  , exec        :: Exec
  , file        :: String
  }

type EmitIL      = Bool
type EmitTL      = Bool
type AsciiLabels = Bool
type EtaMode     = Bool
type Exec        = Bool

----

data Lang = Cat | BF | WS
  deriving (Eq, Read, Show)

langs :: [Lang]
langs = [Cat, BF, WS]

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error ("Lang '" ++ raw ++ "' is not valid lang. Valid langs are : " ++ show langs)

----

data Impl = Monadic | Interact deriving (Eq, Read, Show)

impls :: [Impl]
impls = [Monadic, Interact]

computeImpl :: String -> Impl
computeImpl raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error ("Impl '" ++ raw ++ "' is not valid impl. Valid impls are : " ++ show impls)
