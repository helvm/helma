
Mark (MNatural 1)
SPure (Cons 0)
SPure (Cons 10)
SPure (Cons 33)
SPure (Cons 100)
SPure (Cons 108)
SPure (Cons 114)
SPure (Cons 111)
SPure (Cons 119)
SPure (Cons 32)

Mark (MNatural 2)
SPure (Cons 44)
SPure (Cons 111)
SPure (Cons 108)
SPure (Cons 108)
SPure (Cons 101)
SPure (Cons 72)
SPure (Cons 3)
Labeled (LImmediate 4) Jump

Mark (MNatural 3)
Labeled (LImmediate 0) Jump

Mark (MArtificial "3")
Labeled (LImmediate 4) (Branch NE)
SIO InputChar
SPure (Unary (UImmediate 337 Sub))

Mark (MNatural 4)
SPure (Indexed (IImmediate 1) Move)
SPure (Indexed (IImmediate 0) Copy)
Labeled (LImmediate 6) (Branch NE)

Mark (MNatural 5)
SPure (Binary Sub)
Labeled LTop Jump

Mark (MNatural 6)
SIO OutputChar
Labeled (LImmediate 4) Jump

Mark (MNatural 7)

Mark (MNatural 0)
End
