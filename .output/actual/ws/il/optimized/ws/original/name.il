storeid 80 0
storeid 108 1
storeid 101 2
storeid 97 3
storeid 115 4
storeid 101 5
storeid 32 6
storeid 101 7
storeid 110 8
storeid 116 9
storeid 101 10
storeid 114 11
storeid 32 12
storeid 121 13
storeid 111 14
storeid 117 15
storeid 114 16
storeid 32 17
storeid 110 18
storeid 97 19
storeid 109 20
storeid 101 21
storeid 58 22
storeid 32 23
storeid 0 24
storeid 72 30
storeid 101 31
storeid 108 32
storeid 108 33
storeid 111 34
storeid 32 35
storeid 0 36
cons 0
callA "write"
cons 100
callA "read"
cons 30
callA "write"
cons 100
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
