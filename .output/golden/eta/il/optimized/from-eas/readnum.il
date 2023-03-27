
Mark (MNatural 1)
SIO InputChar

Mark (MNatural 2)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 32 Sub))

Mark (MNatural 3)
Labeled (LImmediate 5) (Branch NE)

Mark (MNatural 4)
Labeled (LImmediate 1) (Branch NE)

Mark (MNatural 5)
SPure (Cons 0)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 6)
SPure (Unary (UImmediate 48 Sub))

Mark (MNatural 7)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 8)
SPure (Cons 10)
SPure (Cons 9)
Labeled (LImmediate 19) Jump

Mark (MNatural 9)
SPure (Cons 0)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 10)
SPure (Binary Sub)
SPure (Binary Sub)

Mark (MNatural 11)
SIO InputChar

Mark (MNatural 12)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 32 Sub))
Labeled (LImmediate 14) (Branch NE)

Mark (MNatural 13)
Labeled (LImmediate 17) Jump

Mark (MNatural 14)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 10 Sub))
Labeled (LImmediate 16) (Branch NE)

Mark (MNatural 15)
Labeled (LImmediate 17) Jump

Mark (MNatural 16)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LImmediate 6) (Branch NE)

Mark (MNatural 17)
SPure Discard

Mark (MNatural 18)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 19)
SPure (Indexed (IImmediate 2) Move)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 20)
SPure (Cons 0)

Mark (MNatural 21)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 22)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 23)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 24)
Labeled (LImmediate 26) (Branch NE)

Mark (MNatural 25)
Labeled (LImmediate 33) Jump

Mark (MNatural 26)
SPure (Unary (UImmediate 1 Sub))

Mark (MNatural 27)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 28)
SPure (Cons 0)
SPure (Indexed (IImmediate 3) Copy)

Mark (MNatural 29)
SPure (Binary Sub)
SPure (Binary Sub)

Mark (MNatural 30)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 31)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 32)
Labeled (LImmediate 23) Jump

Mark (MNatural 33)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop (Branch NE)

Mark (MNatural 34)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 0)
End
