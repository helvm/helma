storeID 69 0
storeID 110 1
storeID 116 2
storeID 101 3
storeID 114 4
storeID 32 5
storeID 115 6
storeID 111 7
storeID 109 8
storeID 101 9
storeID 32 10
storeID 110 11
storeID 117 12
storeID 109 13
storeID 98 14
storeID 101 15
storeID 114 16
storeID 115 17
storeID 44 18
storeID 32 19
storeID 116 20
storeID 104 21
storeID 101 22
storeID 110 23
storeID 32 24
storeID 45 25
storeID 49 26
storeID 32 27
storeID 116 28
storeID 111 29
storeID 32 30
storeID 102 31
storeID 105 32
storeID 110 33
storeID 105 34
storeID 115 35
storeID 104 36
storeID 0 37
storeID 78 42
storeID 117 43
storeID 109 44
storeID 98 45
storeID 101 46
storeID 114 47
storeID 58 48
storeID 0 49
storeID 84 60
storeID 111 61
storeID 116 62
storeID 97 63
storeID 108 64
storeID 32 65
storeID 105 66
storeID 115 67
storeID 32 68
storeID 0 69
cons 0
callA "write"
callA "newline"
storeID 0 100

markA "loop"
cons 42
callA "write"
cons 101
inputD
loadD 101
copy I 0
subI -1
bEZA "progend"
loadD 100
add
cons 100
move I 1
store
jumpA "loop"

markA "progend"
discard
cons 60
callA "write"
loadD 100
outputDS
callA "newline"
end

markA "add"
add
return

markA "write"
copy I 0
load
copy I 0
bEZA "write_end"
outputCS
addI 1
jumpA "write"

markA "write_end"
discard
discard
return

markA "read"
copy I 0
copy I 0
inputC
load
copy I 0
subI 10
bEZA "read_end"
discard
addI 1
jumpA "read"

markA "read_end"
discard
addI 1
storeI 0
return

markA "newline"
cons 10
cons 13
outputCS
outputCS
return
