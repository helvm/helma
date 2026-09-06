module HelVM.HelMA.Automaton.Types.DumpType where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import           Text.Pretty.Simple

logDump ∷ (AppSafeEff m , Show d) ⇒ DumpType → d → m ()
logDump dt d = flip whenJust logInfoNL $ dump dt d

logInfoNL ∷ MonadLogger m ⇒ LText → m ()
logInfoNL  = logWithoutLoc "" LevelInfo . toLogStr

dump ∷ Show a ⇒ DumpType → a → Maybe LText
dump No     _ = Nothing
dump Ugly   a = Just $ show  a
dump Pretty a = Just $ pShowNoColor a

-- | Constructors
defaultDumpType ∷ DumpType
defaultDumpType = minBound

dumpTypes ∷ NonEmpty DumpType
dumpTypes = universeNonEmpty

-- | Types
data DumpType
  = No
  | Ugly
  | Pretty
  deriving stock (Bounded, Enum, Eq, Read, Show)
