
Mark (MNatural 1)
SPure (Cons 2)
Labeled (LImmediate 14) Jump

Mark (MNatural 2)
SPure (Cons 3)
Labeled (LImmediate 5) Jump

Mark (MNatural 3)
SPure (Cons 4)
Labeled (LImmediate 32) Jump

Mark (MNatural 4)
SPure (Cons 10)
SIO OutputChar
Labeled (LImmediate 0) Jump

Mark (MNatural 5)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 6)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 7)
SPure (Unary (UImmediate 1 Sub))
Labeled (LImmediate 9) (Branch NE)

Mark (MNatural 8)
Labeled (LImmediate 13) Jump

Mark (MNatural 9)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 10)
SPure (Unary (UImmediate 1 Sub))

Mark (MNatural 11)
SPure (Cons 12)
Labeled (LImmediate 5) Jump

Mark (MNatural 12)
SPure (Cons 13)
Labeled (LImmediate 42) Jump

Mark (MNatural 13)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 14)
SIO InputChar

Mark (MNatural 15)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 32 Sub))

Mark (MNatural 16)
Labeled (LImmediate 18) (Branch NE)

Mark (MNatural 17)
Labeled (LImmediate 14) (Branch NE)

Mark (MNatural 18)
SPure (Cons 0)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 19)
SPure (Unary (UImmediate 48 Sub))

Mark (MNatural 20)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 21)
SPure (Cons 10)
SPure (Cons 22)
Labeled (LImmediate 42) Jump

Mark (MArtificial "21")
SPure (Cons 22)
SPure (Binaries [Mod,Div])
Labeled LTop (Branch NE)
SIO InputChar
Labeled LTop (Branch NE)
SPure (Binaries [Mod,Div])

Mark (MNatural 22)
SPure (Cons 0)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 23)
SPure (Binary Sub)
SPure (Binary Sub)

Mark (MNatural 24)
SIO InputChar

Mark (MNatural 25)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 32 Sub))
Labeled (LImmediate 27) (Branch NE)

Mark (MNatural 26)
Labeled (LImmediate 30) Jump

Mark (MNatural 27)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 10 Sub))
Labeled (LImmediate 29) (Branch NE)

Mark (MNatural 28)
Labeled (LImmediate 30) Jump

Mark (MNatural 29)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LImmediate 19) (Branch NE)

Mark (MNatural 30)
SPure Discard

Mark (MNatural 31)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 32)
SPure (Cons 0)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 33)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 34)
Labeled (LImmediate 36) (Branch NE)

Mark (MNatural 35)
SPure (Binary Sub)
Labeled (LImmediate 40) Jump

Mark (MNatural 36)
SPure (Cons 10)
SPure (Binaries [Mod,Div])

Mark (MArtificial "36")

Mark (MNatural 37)
SPure (Unary (UImmediate (-48) Sub))

Mark (MNatural 38)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 39)
Labeled (LImmediate 33) Jump

Mark (MNatural 40)
SPure (Cons 41)
Labeled (LImmediate 58) Jump

Mark (MNatural 41)
Labeled LTop Jump

Mark (MNatural 42)
SPure (Indexed (IImmediate 2) Move)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 43)
SPure (Cons 0)

Mark (MNatural 44)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 45)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 46)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 47)
Labeled (LImmediate 49) (Branch NE)

Mark (MNatural 48)
Labeled (LImmediate 56) Jump

Mark (MNatural 49)
SPure (Unary (UImmediate 1 Sub))

Mark (MNatural 50)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 51)
SPure (Cons 0)
SPure (Indexed (IImmediate 3) Copy)

Mark (MNatural 52)
SPure (Binary Sub)
SPure (Binary Sub)

Mark (MNatural 53)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 54)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 55)
Labeled (LImmediate 46) Jump

Mark (MNatural 56)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop (Branch NE)

Mark (MNatural 57)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 58)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 59)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 60)
Labeled (LImmediate 62) (Branch NE)

Mark (MNatural 61)
SPure (Binary Sub)
Labeled LTop Jump

Mark (MNatural 62)
SIO OutputChar

Mark (MNatural 63)
Labeled (LImmediate 58) Jump

Mark (MNatural 64)

Mark (MNatural 0)
End
