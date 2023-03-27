SPure (Cons 0)
SPure (Indexed (IImmediate 0) Copy)
MIO InputDec
Load
Labeled (LArtificial "\NUL") (Branch EZ)

Mark (MArtificial "\SOH")
SPure (Cons 1)
SIO OutputDec
Labeled (LArtificial "\SOH") Jump

Mark (MArtificial "\NUL")
SPure (Cons 0)
SIO OutputDec
End
