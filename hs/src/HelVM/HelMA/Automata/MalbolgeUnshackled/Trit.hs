module HelVM.HelMA.Automata.MalbolgeUnshackled.Trit where

nodeMap :: p -> p -> p -> Trit -> p
nodeMap t0 _ _ T0 = t0
nodeMap _ t1 _ T1 = t1
nodeMap _ _ t2 T2 = t2

-- | The "crazy" operation on trits.
op :: Trit -> Trit -> Trit
op T0 T0 = T1;      
op T1 T0 = T0;      
op T2 T0 = T0
op T0 T1 = T1;      
op T1 T1 = T0;      
op T2 T1 = T2
op T0 T2 = T2;      
op T1 T2 = T2;      
op T2 T2 = T1

data Trit = T0 | T1 | T2
  deriving stock (Enum, Show, Eq)