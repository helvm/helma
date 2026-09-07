
markA "st"
cons 0
cons 93
cons 48
cons 48
cons 49
cons 45
cons 50
cons 91
cons 32
cons 115
cons 114
cons 101
cons 98
cons 109
cons 117
cons 110
cons 32
cons 109
cons 105
cons 114
cons 112
callA "printsln"
cons 0

markA "f_start_1"
copyI 0
subI 100
bEZA "f_finished_1"
copyI 0
addI 100
cons 1
store
addI 1
jumpA "f_start_1"

markA "f_finished_1"
discard
cons 2

markA "f_start_2"
copyI 0
subI 11
bEZA "f_finished_2"
cons 2

markA "f_start_3"
copyI 0
subI 100
bEZA "f_finished_3"
copyI 0
cons 1
moveI 1
store
moveI 1
copyI 0
cons 2
moveI 1
store
moveI 1
loadd 1
loadd 2
mul
addI 100
cons 0
store
addI 1
jumpA "f_start_3"

markA "f_finished_3"
discard
addI 1
jumpA "f_start_2"

markA "f_finished_2"
discard
cons 2

markA "f_start_4"
copyI 0
subI 100
bEZA "f_finished_4"
copyI 0
addI 100
load
bEZA "nodraw"
copyI 0
outputDS
cons 0
cons 32
callA "prints"

markA "nodraw"
addI 1
jumpA "f_start_4"

markA "f_finished_4"
discard
cons 0
callA "printsln"
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
