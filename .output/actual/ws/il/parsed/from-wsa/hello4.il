SPure (Cons 0)
SPure (Cons 33)
SPure (Cons 100)
SPure (Cons 108)
SPure (Cons 114)
SPure (Cons 111)
SPure (Cons 119)
SPure (Cons 32)
SPure (Cons 44)
SPure (Cons 111)
SPure (Cons 108)
SPure (Cons 108)
SPure (Cons 101)
SPure (Cons 72)
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
