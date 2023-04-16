storeid 72 0
storeid 101 1
storeid 108 2
storeid 108 3
storeid 111 4
storeid 44 5
storeid 32 6
storeid 119 7
storeid 111 8
storeid 114 9
storeid 108 10
storeid 100 11
storeid 32 12
storeid 111 13
storeid 102 14
storeid 32 15
storeid 115 16
storeid 112 17
storeid 97 18
storeid 99 19
storeid 101 20
storeid 115 21
storeid 33 22
storeid 0 23
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
addI 1
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
subI 10
bEZA "read_end"
discard
addI 1
jumpA "read"

markA "read_end"
discard
addI 1
cons 0
store
return

markA "newline"
cons 10
cons 13
outputCS
outputCS
return
