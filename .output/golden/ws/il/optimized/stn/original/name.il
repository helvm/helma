storeID 80 0
storeID 108 1
storeID 101 2
storeID 97 3
storeID 115 4
storeID 101 5
storeID 32 6
storeID 101 7
storeID 110 8
storeID 116 9
storeID 101 10
storeID 114 11
storeID 32 12
storeID 121 13
storeID 111 14
storeID 117 15
storeID 114 16
storeID 32 17
storeID 110 18
storeID 97 19
storeID 109 20
storeID 101 21
storeID 58 22
storeID 32 23
storeID 0 24
storeID 72 30
storeID 101 31
storeID 108 32
storeID 108 33
storeID 111 34
storeID 32 35
storeID 0 36
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
