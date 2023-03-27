SPure (Cons 1)

Mark (MArtificial "C")
SPure (Indexed (IImmediate 0) Copy)
SIO OutputDec
SPure (Cons 10)
SIO OutputChar
SPure (Cons 1)
SPure (Binary Add)
SPure (Indexed (IImmediate 0) Copy)
SPure (Cons 11)
SPure (Binary Sub)
Labeled (LArtificial "E") (Branch EZ)
Labeled (LArtificial "C") Jump

Mark (MArtificial "E")
SPure Discard
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
