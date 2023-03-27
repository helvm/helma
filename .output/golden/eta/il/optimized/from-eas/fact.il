
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
Labeled (LImmediate 44) Jump

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
SPure (Cons 16)

Mark (MNatural 20)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 21)
SPure (Cons 10)
SPure (Cons 22)
Labeled (LImmediate 44) Jump

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
Labeled (LImmediate 35) (Branch NE)

Mark (MNatural 34)
SPure (Cons 48)
SIO OutputChar
Labeled LTop (Branch NE)
Labeled LTop Jump

Mark (MNatural 35)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 36)
Labeled (LImmediate 38) (Branch NE)

Mark (MNatural 37)
SPure (Binary Sub)
Labeled (LImmediate 42) Jump

Mark (MNatural 38)
SPure (Cons 10)
SPure (Binaries [Mod,Div])

Mark (MArtificial "38")

Mark (MNatural 39)
SPure (Unary (UImmediate (-48) Sub))

Mark (MNatural 40)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 41)
Labeled (LImmediate 35) Jump

Mark (MNatural 42)
SPure (Cons 43)
Labeled (LImmediate 60) Jump

Mark (MNatural 43)
Labeled LTop Jump

Mark (MNatural 44)
SPure (Indexed (IImmediate 2) Move)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 45)
SPure (Cons 0)

Mark (MNatural 46)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 47)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 48)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 49)
Labeled (LImmediate 51) (Branch NE)

Mark (MNatural 50)
Labeled (LImmediate 58) Jump

Mark (MNatural 51)
SPure (Unary (UImmediate 1 Sub))

Mark (MNatural 52)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 53)
SPure (Cons 0)
SPure (Indexed (IImmediate 3) Copy)

Mark (MNatural 54)
SPure (Binary Sub)
SPure (Binary Sub)

Mark (MNatural 55)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 56)
SPure (Indexed (IImmediate 2) Move)

Mark (MNatural 57)
Labeled (LImmediate 48) Jump

Mark (MNatural 58)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop (Branch NE)

Mark (MNatural 59)
SPure (Indexed (IImmediate 1) Move)
Labeled LTop Jump

Mark (MNatural 60)
SPure (Indexed (IImmediate 1) Move)

Mark (MNatural 61)
SPure (Indexed (IImmediate 0) Copy)

Mark (MNatural 62)
Labeled (LImmediate 64) (Branch NE)

Mark (MNatural 63)
SPure (Binary Sub)
Labeled LTop Jump

Mark (MNatural 64)
SIO OutputChar

Mark (MNatural 65)
Labeled (LImmediate 60) Jump

Mark (MNatural 66)

Mark (MNatural 0)
End
