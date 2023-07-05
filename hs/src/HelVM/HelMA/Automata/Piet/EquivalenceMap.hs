module HelVM.HelMA.Automata.Piet.EquivalenceMap where

import           Data.IntMap hiding (filter)
import           Data.List   (minimum)

equivInsert :: LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert x y mp = let
    class1        = equivClass x mp
    class2        = equivClass y mp
    classes        = [x, y, class1, class2]
    newClass    = minimum classes
    in
    if x /= y
        then fmap (\eqClass -> if or (fmap (== eqClass) classes) then newClass else eqClass)
            $ insert x newClass
            $ insert y newClass mp
        else mp

equivClass :: LabelKey -> EquivalenceMap -> LabelKey
equivClass e = findWithDefault e e

type EquivalenceMap = IntMap LabelKey

type LabelKey = Int
