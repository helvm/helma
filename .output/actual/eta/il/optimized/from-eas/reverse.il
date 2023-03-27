
Mark (MNatural 1)
SPure (Cons (-1))

Mark (MNatural 2)
SIO InputChar

Mark (MNatural 3)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LImmediate 2) (Branch NE)

Mark (MNatural 4)
SPure Discard

Mark (MNatural 5)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LImmediate 7) (Branch NE)

Mark (MNatural 6)
SPure Discard
Labeled (LImmediate 0) Jump

Mark (MNatural 7)
SIO OutputChar
Labeled (LImmediate 5) Jump

Mark (MNatural 8)

Mark (MNatural 0)
End
