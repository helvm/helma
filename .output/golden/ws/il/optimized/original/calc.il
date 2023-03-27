SPure (Cons 0)
SPure (Cons 69)
Store
SPure (Cons 1)
SPure (Cons 110)
Store
SPure (Cons 2)
SPure (Cons 116)
Store
SPure (Cons 3)
SPure (Cons 101)
Store
SPure (Cons 4)
SPure (Cons 114)
Store
SPure (Cons 5)
SPure (Cons 32)
Store
SPure (Cons 6)
SPure (Cons 115)
Store
SPure (Cons 7)
SPure (Cons 111)
Store
SPure (Cons 8)
SPure (Cons 109)
Store
SPure (Cons 9)
SPure (Cons 101)
Store
SPure (Cons 10)
SPure (Cons 32)
Store
SPure (Cons 11)
SPure (Cons 110)
Store
SPure (Cons 12)
SPure (Cons 117)
Store
SPure (Cons 13)
SPure (Cons 109)
Store
SPure (Cons 14)
SPure (Cons 98)
Store
SPure (Cons 15)
SPure (Cons 101)
Store
SPure (Cons 16)
SPure (Cons 114)
Store
SPure (Cons 17)
SPure (Cons 115)
Store
SPure (Cons 18)
SPure (Cons 44)
Store
SPure (Cons 19)
SPure (Cons 32)
Store
SPure (Cons 20)
SPure (Cons 116)
Store
SPure (Cons 21)
SPure (Cons 104)
Store
SPure (Cons 22)
SPure (Cons 101)
Store
SPure (Cons 23)
SPure (Cons 110)
Store
SPure (Cons 24)
SPure (Cons 32)
Store
SPure (Cons 25)
SPure (Cons 45)
Store
SPure (Cons 26)
SPure (Cons 49)
Store
SPure (Cons 27)
SPure (Cons 32)
Store
SPure (Cons 28)
SPure (Cons 116)
Store
SPure (Cons 29)
SPure (Cons 111)
Store
SPure (Cons 30)
SPure (Cons 32)
Store
SPure (Cons 31)
SPure (Cons 102)
Store
SPure (Cons 32)
SPure (Cons 105)
Store
SPure (Cons 33)
SPure (Cons 110)
Store
SPure (Cons 34)
SPure (Cons 105)
Store
SPure (Cons 35)
SPure (Cons 115)
Store
SPure (Cons 36)
SPure (Cons 104)
Store
SPure (Cons 37)
SPure (Cons 0)
Store
SPure (Cons 42)
SPure (Cons 78)
Store
SPure (Cons 43)
SPure (Cons 117)
Store
SPure (Cons 44)
SPure (Cons 109)
Store
SPure (Cons 45)
SPure (Cons 98)
Store
SPure (Cons 46)
SPure (Cons 101)
Store
SPure (Cons 47)
SPure (Cons 114)
Store
SPure (Cons 48)
SPure (Cons 58)
Store
SPure (Cons 49)
SPure (Cons 0)
Store
SPure (Cons 60)
SPure (Cons 84)
Store
SPure (Cons 61)
SPure (Cons 111)
Store
SPure (Cons 62)
SPure (Cons 116)
Store
SPure (Cons 63)
SPure (Cons 97)
Store
SPure (Cons 64)
SPure (Cons 108)
Store
SPure (Cons 65)
SPure (Cons 32)
Store
SPure (Cons 66)
SPure (Cons 105)
Store
SPure (Cons 67)
SPure (Cons 115)
Store
SPure (Cons 68)
SPure (Cons 32)
Store
SPure (Cons 69)
SPure (Cons 0)
Store
SPure (Cons 0)
Labeled (LArtificial "write") Call
Labeled (LArtificial "newline") Call
SPure (Cons 100)
SPure (Cons 0)
Store

Mark (MArtificial "loop")
SPure (Cons 42)
Labeled (LArtificial "write") Call
SPure (Cons 101)
MIO InputDec
SPure (Cons 101)
Load
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate (-1) Sub))
Labeled (LArtificial "progend") (Branch EZ)
SPure (Cons 100)
Load
SPure (Binary Add)
SPure (Cons 100)
SPure (Indexed (IImmediate 1) Move)
Store
Labeled (LArtificial "loop") Jump

Mark (MArtificial "progend")
SPure Discard
SPure (Cons 60)
Labeled (LArtificial "write") Call
SPure (Cons 100)
Load
SIO OutputDec
Labeled (LArtificial "newline") Call
End

Mark (MArtificial "add")
SPure (Binary Add)
Return

Mark (MArtificial "write")
SPure (Indexed (IImmediate 0) Copy)
Load
SPure (Indexed (IImmediate 0) Copy)
Labeled (LArtificial "write_end") (Branch EZ)
SIO OutputChar
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "write") Jump

Mark (MArtificial "write_end")
SPure Discard
SPure Discard
Return

Mark (MArtificial "read")
SPure (Indexed (IImmediate 0) Copy)
SPure (Indexed (IImmediate 0) Copy)
MIO InputChar
Load
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 10 Sub))
Labeled (LArtificial "read_end") (Branch EZ)
SPure Discard
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "read") Jump

Mark (MArtificial "read_end")
SPure Discard
SPure (Unary (UImmediate 1 Add))
SPure (Cons 0)
Store
Return

Mark (MArtificial "newline")
SPure (Cons 10)
SPure (Cons 13)
SIO OutputChar
SIO OutputChar
Return
