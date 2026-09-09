
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
copy I 0
subI 100
bEZA "f_finished_1"
copy I 0
addI 100
storeI 1
addI 1
jumpA "f_start_1"

markA "f_finished_1"
discard
cons 2

markA "f_start_2"
copy I 0
subI 11
bEZA "f_finished_2"
cons 2

markA "f_start_3"
copy I 0
subI 100
bEZA "f_finished_3"
copy I 0
cons 1
move I 1
store
move I 1
copy I 0
cons 2
move I 1
store
move I 1
loadD 1
loadD 2
mul
addI 100
storeI 0
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
copy I 0
subI 100
bEZA "f_finished_4"
copy I 0
addI 100
load
bEZA "nodraw"
copy I 0
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
copy I 0
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
