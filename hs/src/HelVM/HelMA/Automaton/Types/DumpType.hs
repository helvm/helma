module HelVM.HelMA.Automaton.Types.DumpType where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import qualified Data.Text.Lazy                     as LT
import qualified Data.Text.Lazy.Builder             as B
import           Text.Pretty.Simple                 ( pShowNoColor )

logDump ∷ (AppSafeEff m, Show d) ⇒ DumpType → d → m ()
logDump No     _ = pass
logDump Ugly   d = logInfoNL $ B.toLazyText $ B.fromString $ show d
logDump Pretty d = logInfoNL $ pShowNoColor d
{-# INLINE logDump #-}

logInfoNL ∷ MonadLogger m ⇒ LT.Text → m ()
logInfoNL = logWithoutLoc "" LevelInfo . toLogStr
{-# INLINE logInfoNL #-}

dump ∷ Show a ⇒ DumpType → a → Maybe LT.Text
dump No     _ = Nothing
dump Ugly   a = Just $ B.toLazyText $ B.fromString $ show a
dump Pretty a = Just $ pShowNoColor a
{-# INLINABLE dump #-}

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
