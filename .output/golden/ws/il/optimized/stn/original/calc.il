storeid 69 0
storeid 110 1
storeid 116 2
storeid 101 3
storeid 114 4
storeid 32 5
storeid 115 6
storeid 111 7
storeid 109 8
storeid 101 9
storeid 32 10
storeid 110 11
storeid 117 12
storeid 109 13
storeid 98 14
storeid 101 15
storeid 114 16
storeid 115 17
storeid 44 18
storeid 32 19
storeid 116 20
storeid 104 21
storeid 101 22
storeid 110 23
storeid 32 24
storeid 45 25
storeid 49 26
storeid 32 27
storeid 116 28
storeid 111 29
storeid 32 30
storeid 102 31
storeid 105 32
storeid 110 33
storeid 105 34
storeid 115 35
storeid 104 36
storeid 0 37
storeid 78 42
storeid 117 43
storeid 109 44
storeid 98 45
storeid 101 46
storeid 114 47
storeid 58 48
storeid 0 49
storeid 84 60
storeid 111 61
storeid 116 62
storeid 97 63
storeid 108 64
storeid 32 65
storeid 105 66
storeid 115 67
storeid 32 68
storeid 0 69
cons 0
callA "write"
callA "newline"
storeid 0 100

markA "loop"
cons 42
callA "write"
cons 101
mio inputdec
loadd 101
copyI 0
subI -1
bEZA "progend"
loadd 100
add
cons 100
moveI 1
store
jumpA "loop"

markA "progend"
discard
cons 60
callA "write"
loadd 100
outputDS
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
