module HelVM.HelMA.Automaton.Types.DumpType where

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Control.Logger

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

logDump :: (BIO m , Show d) => DumpType -> d -> m ()
logDump dt d = logDump' $ dump dt d where
  logDump' Nothing  = pass
  logDump' (Just t) = logMessageTuple ("dump" , t)

dump :: Show a => DumpType -> a -> Maybe Text
dump No     _ = Nothing
dump Ugly   a = Just $ show  a
dump Pretty a = Just $ showP a

-- | Constructors
defaultDumpType :: DumpType
defaultDumpType = defaultEnum

dumpTypes :: [DumpType]
dumpTypes = generateEnums 3

-- | Types
data DumpType = No | Ugly | Pretty
  deriving stock (Bounded , Enum , Eq , Read , Show)
