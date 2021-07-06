[ Liter 1
, Mark "C"
, Dup
, OutputNum
, Liter 10
, OutputChar
, Liter 1
, Binary Add
, Dup
, Liter 11
, Binary Sub
, Branch EZ "E"
, Jump "C"
, Mark "E"
, Discard
, End
, Mark "add"
, Binary Add
, Return
, Mark "write"
, Dup
, Load
, Dup
, Branch EZ "write_end"
, OutputChar
, Liter 1
, Binary Add
, Jump "write"
, Mark "write_end"
, Discard
, Discard
, Return
, Mark "read"
, Dup
, Dup
, InputChar
, Load
, Dup
, Liter 10
, Binary Sub
, Branch EZ "read_end"
, Discard
, Liter 1
, Binary Add
, Jump "read"
, Mark "read_end"
, Discard
, Liter 1
, Binary Add
, Liter 0
, Store
, Return
, Mark "newline"
, Liter 10
, Liter 13
, OutputChar
, OutputChar
, Return
]