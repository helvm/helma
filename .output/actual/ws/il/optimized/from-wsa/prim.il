
Mark (MArtificial "st")
SPure (Cons 0)
SPure (Cons 93)
SPure (Cons 48)
SPure (Cons 48)
SPure (Cons 49)
SPure (Cons 45)
SPure (Cons 50)
SPure (Cons 91)
SPure (Cons 32)
SPure (Cons 115)
SPure (Cons 114)
SPure (Cons 101)
SPure (Cons 98)
SPure (Cons 109)
SPure (Cons 117)
SPure (Cons 110)
SPure (Cons 32)
SPure (Cons 109)
SPure (Cons 105)
SPure (Cons 114)
SPure (Cons 112)
Labeled (LArtificial "printsln") Call
SPure (Cons 0)

Mark (MArtificial "f_start_1")
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 100 Sub))
Labeled (LArtificial "f_finished_1") (Branch EZ)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 100 Add))
SPure (Cons 1)
Store
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "f_start_1") Jump

Mark (MArtificial "f_finished_1")
SPure Discard
SPure (Cons 2)

Mark (MArtificial "f_start_2")
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 11 Sub))
Labeled (LArtificial "f_finished_2") (Branch EZ)
SPure (Cons 2)

Mark (MArtificial "f_start_3")
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 100 Sub))
Labeled (LArtificial "f_finished_3") (Branch EZ)
SPure (Indexed (IImmediate 0) Copy)
SPure (Cons 1)
SPure (Indexed (IImmediate 1) Move)
Store
SPure (Indexed (IImmediate 1) Move)
SPure (Indexed (IImmediate 0) Copy)
SPure (Cons 2)
SPure (Indexed (IImmediate 1) Move)
Store
SPure (Indexed (IImmediate 1) Move)
SPure (Cons 1)
Load
SPure (Cons 2)
Load
SPure (Binary Mul)
SPure (Unary (UImmediate 100 Add))
SPure (Cons 0)
Store
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "f_start_3") Jump

Mark (MArtificial "f_finished_3")
SPure Discard
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "f_start_2") Jump

Mark (MArtificial "f_finished_2")
SPure Discard
SPure (Cons 2)

Mark (MArtificial "f_start_4")
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 100 Sub))
Labeled (LArtificial "f_finished_4") (Branch EZ)
SPure (Indexed (IImmediate 0) Copy)
SPure (Unary (UImmediate 100 Add))
Load
Labeled (LArtificial "nodraw") (Branch EZ)
SPure (Indexed (IImmediate 0) Copy)
SIO OutputDec
SPure (Cons 0)
SPure (Cons 32)
Labeled (LArtificial "prints") Call

Mark (MArtificial "nodraw")
SPure (Unary (UImmediate 1 Add))
Labeled (LArtificial "f_start_4") Jump

Mark (MArtificial "f_finished_4")
SPure Discard
SPure (Cons 0)
Labeled (LArtificial "printsln") Call
End

Mark (MArtificial "prints")
SPure (Indexed (IImmediate 0) Copy)
Labeled (LArtificial "prints_end") (Branch EZ)
SIO OutputChar
Labeled (LArtificial "prints") Jump

Mark (MArtificial "prints_end")
SPure Discard
Return

Mark (MArtificial "printsln")
Labeled (LArtificial "prints") Call
SPure (Cons 10)
SIO OutputChar
Return
