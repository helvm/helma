module HelVM.HelMA.Automata.ETA.API.ETAImplType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultETAImplType :: ETAImplType
defaultETAImplType = defaultEnum

etaImplTypes:: [ETAImplType]
etaImplTypes = generateEnums 2

-- | Type
data ETAImplType = Fast | Original
  deriving stock (Bounded , Enum , Eq , Read , Show)
