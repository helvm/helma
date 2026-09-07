storeid 69 0
storeid 110 1
storeid 116 2
storeid 101 3
storeid 114 4
storeid 32 5
storeid 97 6
storeid 32 7
storeid 110 8
storeid 117 9
storeid 109 10
storeid 98 11
storeid 101 12
storeid 114 13
storeid 58 14
storeid 32 15
storeid 0 16
storeid 33 20
storeid 32 21
storeid 61 22
storeid 32 23
storeid 0 24
cons 0
callA "write"
cons 100
mio inputdec
loadd 100
callA "fact"
loadd 100
outputDS
cons 20
callA "write"
outputDS
callA "newline"
end

markA "fact"
copyI 0
subI 1
bEZA "factbase"
copyI 0
subI 1
callA "fact"
mul
return

markA "factbase"
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
