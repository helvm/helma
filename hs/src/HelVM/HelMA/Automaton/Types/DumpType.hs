module HelVM.HelMA.Automaton.Types.DumpType where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Extra

import           Control.Monad.Logger

logDump :: (AppEff m , Show d) => DumpType -> d -> m ()
logDump dt d = logDump' $ dump dt d where
  logDump' Nothing  = pass
  logDump' (Just t) = logInfoN $ logTupleToMessage ("dump" , t)
  logTupleToMessage (k , v) = k <> ": " <> v

dump :: Show a => DumpType -> a -> Maybe Text
dump No     _ = Nothing
dump Ugly   a = Just $ show  a
dump Pretty a = Just $ showP a

-- | Constructors
defaultDumpType :: DumpType
defaultDumpType = minBound

dumpTypes :: NonEmpty DumpType
dumpTypes = universeNonEmpty

-- | Types
data DumpType = No | Ugly | Pretty
  deriving stock (Bounded , Enum , Eq , Read , Show)
