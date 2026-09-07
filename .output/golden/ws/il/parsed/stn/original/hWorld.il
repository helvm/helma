cons 0
cons 72
store
cons 1
cons 101
store
cons 2
cons 108
store
cons 3
cons 108
store
cons 4
cons 111
store
cons 5
cons 44
store
cons 6
cons 32
store
cons 7
cons 119
store
cons 8
cons 111
store
cons 9
cons 114
store
cons 10
cons 108
store
cons 11
cons 100
store
cons 12
cons 32
store
cons 13
cons 111
store
cons 14
cons 102
store
cons 15
cons 32
store
cons 16
cons 115
store
cons 17
cons 112
store
cons 18
cons 97
store
cons 19
cons 99
store
cons 20
cons 101
store
cons 21
cons 115
store
cons 22
cons 33
store
cons 23
cons 0
store
cons 0
callA "write"
callA "newline"
end

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
