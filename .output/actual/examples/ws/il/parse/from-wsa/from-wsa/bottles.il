[ Liter 3
, Mark "LOOP"
, Dup
, OutputNum
, Liter 0
, Liter 32
, Liter 44
, Liter 108
, Liter 108
, Liter 97
, Liter 119
, Liter 32
, Liter 101
, Liter 104
, Liter 116
, Liter 32
, Liter 110
, Liter 111
, Liter 32
, Liter 114
, Liter 101
, Liter 101
, Liter 98
, Liter 32
, Liter 102
, Liter 111
, Liter 32
, Liter 115
, Liter 101
, Liter 108
, Liter 116
, Liter 116
, Liter 111
, Liter 98
, Liter 32
, Call "prints"
, Dup
, OutputNum
, Liter 0
, Liter 114
, Liter 101
, Liter 101
, Liter 98
, Liter 32
, Liter 102
, Liter 111
, Liter 32
, Liter 115
, Liter 101
, Liter 108
, Liter 116
, Liter 116
, Liter 111
, Liter 98
, Liter 32
, Call "printsln"
, Liter 0
, Liter 100
, Liter 110
, Liter 117
, Liter 111
, Liter 114
, Liter 97
, Liter 32
, Liter 116
, Liter 105
, Liter 32
, Liter 115
, Liter 115
, Liter 97
, Liter 112
, Liter 32
, Liter 44
, Liter 110
, Liter 119
, Liter 111
, Liter 100
, Liter 32
, Liter 101
, Liter 110
, Liter 111
, Liter 32
, Liter 101
, Liter 107
, Liter 97
, Liter 84
, Call "printsln"
, Liter 1
, Binary Sub
, Dup
, OutputNum
, Liter 0
, Liter 108
, Liter 108
, Liter 97
, Liter 119
, Liter 32
, Liter 101
, Liter 104
, Liter 116
, Liter 32
, Liter 110
, Liter 111
, Liter 32
, Liter 114
, Liter 101
, Liter 101
, Liter 98
, Liter 32
, Liter 102
, Liter 111
, Liter 32
, Liter 115
, Liter 101
, Liter 108
, Liter 116
, Liter 116
, Liter 111
, Liter 98
, Liter 32
, Call "printsln"
, Liter 0
, Call "printsln"
, Dup
, Branch EZ "LOOP:1"
, Jump "LOOP"
, Mark "LOOP:1"
, End
, Mark "prints"
, Dup
, Branch EZ "prints_end"
, OutputChar
, Jump "prints"
, Mark "prints_end"
, Discard
, Return
, Mark "printsln"
, Call "prints"
, Liter 10
, OutputChar
, Return
]