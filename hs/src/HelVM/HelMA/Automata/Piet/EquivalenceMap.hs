module HelVM.HelMA.Automata.Piet.EquivalenceMap where

import           Data.IntMap hiding (filter)
import           Data.List   (minimum)

equivInsert :: LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert x y = equivInsert' (x /= y) x y

equivInsert' :: Bool -> LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert' False _ _ mp = mp
equivInsert' True  x y mp = (getClass classes newClass) <$> (insert2 x y newClass mp) where
  newClass = minimum classes
  classes  = [x, y, classX, classY]
  classX   = equivClass x mp
  classY   = equivClass y mp

insert2 :: Key -> Key -> a -> IntMap a -> IntMap a
insert2 key2 key1 value = insert key2 value . insert key1 value

getClass :: (Foldable f, Functor f, Eq a) => f a -> a -> a -> a
getClass classes newClass eqClass = getClass' $ or ((== eqClass) <$> classes) where
  getClass' True  = newClass
  getClass' False = eqClass

equivClass :: LabelKey -> EquivalenceMap -> LabelKey
equivClass e = findWithDefault e e

type EquivalenceMap = IntMap LabelKey

type LabelKey = Int
