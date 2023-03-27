
Mark (MNatural 1)
SIO InputChar
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LImmediate 2) (Branch NE)
SPure Discard
Labeled (LImmediate 0) Jump

Mark (MNatural 2)
SIO OutputChar
Labeled (LImmediate 1) Jump

Mark (MNatural 3)

Mark (MNatural 0)
End
