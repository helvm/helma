cons 3

markA "LOOP"
copyI 0
outputDS
cons 0
cons 32
cons 44
cons 108
cons 108
cons 97
cons 119
cons 32
cons 101
cons 104
cons 116
cons 32
cons 110
cons 111
cons 32
cons 114
cons 101
cons 101
cons 98
cons 32
cons 102
cons 111
cons 32
cons 115
cons 101
cons 108
cons 116
cons 116
cons 111
cons 98
cons 32
callA "prints"
copyI 0
outputDS
cons 0
cons 114
cons 101
cons 101
cons 98
cons 32
cons 102
cons 111
cons 32
cons 115
cons 101
cons 108
cons 116
cons 116
cons 111
cons 98
cons 32
callA "printsln"
cons 0
cons 100
cons 110
cons 117
cons 111
cons 114
cons 97
cons 32
cons 116
cons 105
cons 32
cons 115
cons 115
cons 97
cons 112
cons 32
cons 44
cons 110
cons 119
cons 111
cons 100
cons 32
cons 101
cons 110
cons 111
cons 32
cons 101
cons 107
cons 97
cons 84
callA "printsln"
subI 1
copyI 0
outputDS
cons 0
cons 108
cons 108
cons 97
cons 119
cons 32
cons 101
cons 104
cons 116
cons 32
cons 110
cons 111
cons 32
cons 114
cons 101
cons 101
cons 98
cons 32
cons 102
cons 111
cons 32
cons 115
cons 101
cons 108
cons 116
cons 116
cons 111
cons 98
cons 32
callA "printsln"
cons 0
callA "printsln"
copyI 0
bEZA "LOOP:1"
jumpA "LOOP"

markA "LOOP:1"
end

markA "prints"
copyI 0
bEZA "prints_end"
outputCS
jumpA "prints"

markA "prints_end"
discard
return

markA "printsln"
callA "prints"
cons 10
outputCS
return
