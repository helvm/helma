storeID 72 0
storeID 101 1
storeID 108 2
storeID 108 3
storeID 111 4
storeID 44 5
storeID 32 6
storeID 119 7
storeID 111 8
storeID 114 9
storeID 108 10
storeID 100 11
storeID 32 12
storeID 111 13
storeID 102 14
storeID 32 15
storeID 115 16
storeID 112 17
storeID 97 18
storeID 99 19
storeID 101 20
storeID 115 21
storeID 33 22
storeID 0 23
cons 0
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
