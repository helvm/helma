
Mark (MNatural 1)
SPure (Cons 0)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 2)
SPure (Indexed (IImmediate 0) Copy)
Labeled (LImmediate 4) (Branch NE)

Mark (MNatural 3)
SPure (Cons 48)
SIO OutputChar
Labeled LTop (Branch NE)
Labeled LTop Jump

Mark (MNatural 4)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 5)
Labeled (LImmediate 7) (Branch NE)

Mark (MNatural 6)
SPure (Binary Sub)
Labeled (LImmediate 11) Jump

Mark (MNatural 7)
SPure (Cons 10)
SPure (Binaries [Mod,Div])

Mark (MArtificial "7")

Mark (MNatural 8)
SPure (Unary (UImmediate (-48) Sub))

Mark (MNatural 9)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 10)
Labeled (LImmediate 4) Jump

Mark (MNatural 11)
SPure (Cons 12)
Labeled (LImmediate 13) Jump

Mark (MNatural 12)
Labeled LTop Jump

Mark (MNatural 13)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 14)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 15)
Labeled (LImmediate 17) (Branch NE)

Mark (MNatural 16)
SPure (Binary Sub)
Labeled LTop Jump

Mark (MNatural 17)
SIO OutputChar

Mark (MNatural 18)
Labeled (LImmediate 13) Jump

Mark (MNatural 19)

Mark (MNatural 0)
End
