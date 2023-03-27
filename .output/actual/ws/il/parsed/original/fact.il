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
SPure (Cons 97)
Store
SPure (Cons 7)
SPure (Cons 32)
Store
SPure (Cons 8)
SPure (Cons 110)
Store
SPure (Cons 9)
SPure (Cons 117)
Store
SPure (Cons 10)
SPure (Cons 109)
Store
SPure (Cons 11)
SPure (Cons 98)
Store
SPure (Cons 12)
SPure (Cons 101)
Store
SPure (Cons 13)
SPure (Cons 114)
Store
SPure (Cons 14)
SPure (Cons 58)
Store
SPure (Cons 15)
SPure (Cons 32)
Store
SPure (Cons 16)
SPure (Cons 0)
Store
SPure (Cons 20)
SPure (Cons 33)
Store
SPure (Cons 21)
SPure (Cons 32)
Store
SPure (Cons 22)
SPure (Cons 61)
Store
SPure (Cons 23)
SPure (Cons 32)
Store
SPure (Cons 24)
SPure (Cons 0)
Store
SPure (Cons 0)
Labeled (LArtificial "write") Call
SPure (Cons 100)
MIO InputDec
SPure (Cons 100)
Load
Labeled (LArtificial "fact") Call
SPure (Cons 100)
Load
SIO OutputDec
SPure (Cons 20)
Labeled (LArtificial "write") Call
SIO OutputDec
Labeled (LArtificial "newline") Call
End

Mark (MArtificial "fact")
SPure (Indexed (IImmediate 0) Copy)
SPure (Cons 1)
SPure (Binary Sub)
Labeled (LArtificial "factbase") (Branch EZ)
SPure (Indexed (IImmediate 0) Copy)
SPure (Cons 1)
SPure (Binary Sub)
Labeled (LArtificial "fact") Call
SPure (Binary Mul)
Return

Mark (MArtificial "factbase")
SPure (Cons 1)
SPure Discard
Return

Mark (MArtificial "add")
SPure (Binary Add)
Return

Mark (MArtificial "write")
SPure (Indexed (IImmediate 0) Copy)
Load
SPure (Indexed (IImmediate 0) Copy)
Labeled (LArtificial "write_end") (Branch EZ)
SIO OutputChar
SPure (Cons 1)
SPure (Binary Add)
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
SPure (Cons 10)
SPure (Binary Sub)
Labeled (LArtificial "read_end") (Branch EZ)
SPure Discard
SPure (Cons 1)
SPure (Binary Add)
Labeled (LArtificial "read") Jump

Mark (MArtificial "read_end")
SPure Discard
SPure (Cons 1)
SPure (Binary Add)
SPure (Cons 0)
Store
Return

Mark (MArtificial "newline")
SPure (Cons 10)
SPure (Cons 13)
SIO OutputChar
SIO OutputChar
Return
