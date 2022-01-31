module HelVM.HelMA.Automaton.Types.DumpType where

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.Control.Logger

import           HelVM.Common.Util

logDump :: (BIO m , Show d) => DumpType -> d -> m ()
logDump dt d = logDump' $ dump dt d where
  logDump' Nothing  = pass
  logDump' (Just t) = logMessageTuple ("dump" , t)

dump :: Show a => DumpType -> a -> Maybe Text
dump No     _ = Nothing
dump Ugly   a = Just $ show  a
dump Pretty a = Just $ showP a

-- | Constructors
parseDumpType :: String -> DumpType
parseDumpType raw = (valid . readMaybe) raw where
  valid (Just value) = value
  valid Nothing      = error $ "DumpType '" <> toText raw <> "' is not valid DumpType. Valid dumpTypes are : " <> show dumpTypes

defaultDumpType :: DumpType
defaultDumpType = No

dumpTypes :: [DumpType]
dumpTypes = [No , Ugly , Pretty]

-- | Types
data DumpType = No | Ugly | Pretty
  deriving stock (Eq , Read , Show)
