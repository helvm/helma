storeID 69 0
storeID 110 1
storeID 116 2
storeID 101 3
storeID 114 4
storeID 32 5
storeID 97 6
storeID 32 7
storeID 110 8
storeID 117 9
storeID 109 10
storeID 98 11
storeID 101 12
storeID 114 13
storeID 58 14
storeID 32 15
storeID 0 16
cons 0
callA "write"
cons 100
inputD
cons 0
callA "write"
cons 101
inputD
loadD 100
loadD 101
callA "loctest"
outputDS
end

markA "loctest"
copy I 0
copy I 2
add
slide I 2
return

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
