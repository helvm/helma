cons 0
cons 69
store
cons 1
cons 110
store
cons 2
cons 116
store
cons 3
cons 101
store
cons 4
cons 114
store
cons 5
cons 32
store
cons 6
cons 97
store
cons 7
cons 32
store
cons 8
cons 110
store
cons 9
cons 117
store
cons 10
cons 109
store
cons 11
cons 98
store
cons 12
cons 101
store
cons 13
cons 114
store
cons 14
cons 58
store
cons 15
cons 32
store
cons 16
cons 0
store
cons 0
callA "write"
cons 100
mio inputdec
cons 0
callA "write"
cons 101
mio inputdec
cons 100
load
cons 101
load
callA "loctest"
outputDS
end

markA "loctest"
copyI 0
copyI 2
add
slideI 2
return

markA "add"
add
return

markA "write"
copyI 0
load
copyI 0
bEZA "write_end"
outputCS
cons 1
add
jumpA "write"

markA "write_end"
discard
discard
return

markA "read"
copyI 0
copyI 0
mio inputchar
load
copyI 0
cons 10
sub
bEZA "read_end"
discard
cons 1
add
jumpA "read"

markA "read_end"
discard
cons 1
add
cons 0
store
return

markA "newline"
cons 10
cons 13
outputCS
outputCS
return
