module HelVM.HelMA.Automata.Piet.EquivalenceMap where

import           Data.IntMap hiding (filter)
import           Data.List   (minimum)

equivInsert :: LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert x y = equivInsert' (x /= y) x y

equivInsert' :: Bool -> LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert' False _ _ mp = mp
equivInsert' True  x y mp = fmap (bbb classes newClass) $ insert x newClass $ insert y newClass mp where
  newClass    = minimum classes
  classes        = [x, y, classX, classY]
  classX        = equivClass x mp
  classY        = equivClass y mp

bbb :: (Foldable f, Functor f, Eq a) => f a -> a -> a -> a
bbb classes newClass eqClass = r $ or (fmap (== eqClass) classes) where
  r True  = newClass
  r False = eqClass

equivClass :: LabelKey -> EquivalenceMap -> LabelKey
equivClass e = findWithDefault e e

type EquivalenceMap = IntMap LabelKey

type LabelKey = Int
