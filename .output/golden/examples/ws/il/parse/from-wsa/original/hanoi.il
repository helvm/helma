[ Liter 0
, Liter 69
, Store
, Liter 1
, Liter 110
, Store
, Liter 2
, Liter 116
, Store
, Liter 3
, Liter 101
, Store
, Liter 4
, Liter 114
, Store
, Liter 5
, Liter 32
, Store
, Liter 6
, Liter 97
, Store
, Liter 7
, Liter 32
, Store
, Liter 8
, Liter 110
, Store
, Liter 9
, Liter 117
, Store
, Liter 10
, Liter 109
, Store
, Liter 11
, Liter 98
, Store
, Liter 12
, Liter 101
, Store
, Liter 13
, Liter 114
, Store
, Liter 14
, Liter 58
, Store
, Liter 15
, Liter 32
, Store
, Liter 16
, Liter 0
, Store
, Liter 20
, Liter 32
, Store
, Liter 21
, Liter 45
, Store
, Liter 22
, Liter 62
, Store
, Liter 23
, Liter 32
, Store
, Liter 24
, Liter 0
, Store
, Liter 0
, Call "write"
, Liter 100
, InputNum
, Liter 100
, Load
, Liter 1
, Liter 3
, Liter 2
, Call "hanoi"
, End
, Mark "hanoi"
, Liter 103
, Swap
, Store
, Liter 102
, Swap
, Store
, Liter 101
, Swap
, Store
, Liter 100
, Swap
, Store
, Liter 100
, Load
, Branch EZ "endhanoi"
, Liter 100
, Load
, Liter 101
, Load
, Liter 102
, Load
, Liter 103
, Load
, Liter 100
, Load
, Liter 1
, Binary Sub
, Liter 101
, Load
, Liter 103
, Load
, Liter 102
, Load
, Call "hanoi"
, Liter 103
, Swap
, Store
, Liter 102
, Swap
, Store
, Liter 101
, Swap
, Store
, Liter 100
, Swap
, Store
, Liter 101
, Load
, OutputNum
, Liter 20
, Call "write"
, Liter 102
, Load
, OutputNum
, Call "newline"
, Liter 100
, Load
, Liter 101
, Load
, Liter 102
, Load
, Liter 103
, Load
, Liter 100
, Load
, Liter 1
, Binary Sub
, Liter 103
, Load
, Liter 102
, Load
, Liter 101
, Load
, Call "hanoi"
, Liter 103
, Swap
, Store
, Liter 102
, Swap
, Store
, Liter 101
, Swap
, Store
, Liter 100
, Swap
, Store
, Mark "endhanoi"
, Return
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