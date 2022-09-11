module HelVM.HelMA.Automata.MalbolgeUnshackled.Value where

import HelVM.HelMA.Automata.MalbolgeUnshackled.Trit
import HelVM.HelMA.Automata.MalbolgeUnshackled.Util

import Prelude hiding ( bug )

import           Data.Array

charToValue :: Char -> Value
charToValue = OffsetV T0 . toInteger . ord

encrypt :: Array Integer Value
encrypt = listArray (33 :: Integer, 126) $ map charToValue
    "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1C\
    \B6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@"

-- | Might be used for consistency check
vCheck :: Value -> Bool
vCheck (OffsetV T0 offset) = offset >= 0
vCheck (OffsetV T2 offset) = offset <= 0
vCheck (ListV [])          = False
vCheck _                   = True

-- | Conversion between value representations
vToOffset :: Value -> Value
vToOffset val@(OffsetV _ _) = val
vToOffset (ListV []) = bug "Empty tritlist"
vToOffset (ListV l) = OffsetV base offset where
    (base , rest) = splitList $ reverse l
    offset = foldl' collect 0 rest
    collect x t = x*3 + toInteger (fromEnum t - fromEnum base)
    
splitList :: [a] -> (a, [a])
splitList (base : rest) = (base , rest)
splitList [] = error "empty list"

opValue :: Value -> Value -> Value
opValue v1 v2 = ListV $ compressList $ opv l1 l2 where
    opv [t] l           = map (t `op`) l
    opv l [t]           = map (`op` t) l
    opv (t1:r1) (t2:r2) = (t1 `op` t2):opv r1 r2
    opv _ _             = bug "opv given empty ListV"
    ListV l1 = vToList v1
    ListV l2 = vToList v2

rotate :: (Num a, Ord a) => a -> Value -> Value
rotate width val = ListV $ compressList $ rot w0 t0 r0 where
    w0 = if width >= 1 then width else bug "Rotation width not positive"
    ListV (t0 : r') = vToList val
    r0 = case r' of
        [] -> [t0]
        _  -> r'
    rot 1 t r      = t : r
    rot w t [l]    = l : rot (w-1) t [l]
    rot w t (t':l) = t' : rot (w-1) t l
    
vToList :: Value -> Value
vToList val@(ListV _) = val
vToList (OffsetV base offset) = ListV (buildList offset) where
    b = toInteger (fromEnum base)
    buildList 0 = [base]
    buildList o = toEnum (fromInteger m) : buildList d where
        (d,m) = (o+b) `divMod` 3

data Value = OffsetV Trit Integer | ListV [Trit] deriving stock (Show)
