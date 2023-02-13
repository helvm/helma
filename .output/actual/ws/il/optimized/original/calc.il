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
cons 115
store
cons 7
cons 111
store
cons 8
cons 109
store
cons 9
cons 101
store
cons 10
cons 32
store
cons 11
cons 110
store
cons 12
cons 117
store
cons 13
cons 109
store
cons 14
cons 98
store
cons 15
cons 101
store
cons 16
cons 114
store
cons 17
cons 115
store
cons 18
cons 44
store
cons 19
cons 32
store
cons 20
cons 116
store
cons 21
cons 104
store
cons 22
cons 101
store
cons 23
cons 110
store
cons 24
cons 32
store
cons 25
cons 45
store
cons 26
cons 49
store
cons 27
cons 32
store
cons 28
cons 116
store
cons 29
cons 111
store
cons 30
cons 32
store
cons 31
cons 102
store
cons 32
cons 105
store
cons 33
cons 110
store
cons 34
cons 105
store
cons 35
cons 115
store
cons 36
cons 104
store
cons 37
cons 0
store
cons 42
cons 78
store
cons 43
cons 117
store
cons 44
cons 109
store
cons 45
cons 98
store
cons 46
cons 101
store
cons 47
cons 114
store
cons 48
cons 58
store
cons 49
cons 0
store
cons 60
cons 84
store
cons 61
cons 111
store
cons 62
cons 116
store
cons 63
cons 97
store
cons 64
cons 108
store
cons 65
cons 32
store
cons 66
cons 105
store
cons 67
cons 115
store
cons 68
cons 32
store
cons 69
cons 0
store
cons 0
callA "write"
callA "newline"
cons 100
cons 0
store

markA "loop"
cons 42
callA "write"
cons 101
mio inputdec
cons 101
load
copyI 0
subI -1
bEZA "progend"
cons 100
load
add
cons 100
moveI 1
store
jumpA "loop"

markA "progend"
discard
cons 60
callA "write"
cons 100
load
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
