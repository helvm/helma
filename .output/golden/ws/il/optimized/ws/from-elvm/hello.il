storeID 0 0
storeID 0 1
storeID 0 2
storeID 0 3
storeID 0 4
storeID 0 5
storeID 0 6
storeID 72 8
storeID 101 9
storeID 108 10
storeID 108 11
storeID 111 12
storeID 44 13
storeID 32 14
storeID 119 15
storeID 111 16
storeID 114 17
storeID 108 18
storeID 100 19
storeID 33 20
storeID 10 21
storeID 0 22
storeID 16 23

markA "00"
jumpA "01"

markA "01"
moveD 5 3
cons 3
loadD 3
addI 33554431
modI 16777216
store
loadD 3
addI 8
loadD 4
store
moveD 3 5
moveD 5 4
cons 5
loadsubDI 5 1
addI 16777216
modI 16777216
store
storeID 0 0
moveD 5 1
storeID 0 0
moveD 4 1
cons 1
loadD 1
addI 33554431
modI 16777216
store
loadD 1
addI 8
loadD 0
store

markA "010"
moveD 4 1
cons 1
loadD 1
addI 33554431
modI 16777216
store
cons 0
loadD 1
addI 8
load
store
moveD 0 1
cons 0
loadD 1
addI 8
load
store
loadD 0
bEZA "01001"
jumpA "01010"

markA "01010"
cons 1
jumpA "01011"

markA "01001"
cons 0

markA "01011"
bEZA "0100"

markA "011"
jumpA "0101"

markA "0100"
jumpA "0111"

markA "0101"
moveD 4 1
cons 1
loadD 1
addI 33554431
modI 16777216
store
cons 0
loadD 1
addI 8
load
store
moveD 0 1
cons 0
loadD 1
addI 8
load
store
moveD 5 3
cons 3
loadD 3
addI 33554431
modI 16777216
store
loadD 3
addI 8
loadD 0
store
moveD 3 5
loadD 0
outputCS
cons 5
loadD 5
addI 16777217
modI 16777216
store

markA "0110"
moveD 4 1
cons 1
loadD 1
addI 33554431
modI 16777216
store
cons 0
loadD 1
addI 8
load
store
moveD 5 3
cons 3
loadD 3
addI 33554431
modI 16777216
store
loadD 3
addI 8
loadD 0
store
moveD 3 5
cons 0
loadD 0
addI 16777217
modI 16777216
store
moveD 4 1
cons 1
loadD 1
addI 33554431
modI 16777216
store
loadD 1
addI 8
loadD 0
store
cons 0
loadD 5
addI 8
load
store
cons 5
loadD 5
addI 16777217
modI 16777216
store
jumpA "010"

markA "0111"
storeID 0 0
moveD 0 1
end
end

markA "01000"
copy I 0
subI 4
bLTZA "01111"
copy I 0
subI 6
bLTZA "010001"
copy I 0
subI 7
bLTZA "010010"
discard
jumpA "0111"

markA "010010"
discard
jumpA "0110"

markA "010001"
copy I 0
subI 5
bLTZA "010000"
discard
jumpA "0101"

markA "010000"
discard
jumpA "0100"

markA "01111"
copy I 0
subI 2
bLTZA "01101"
copy I 0
subI 3
bLTZA "01110"
discard
jumpA "011"

markA "01110"
discard
jumpA "010"

markA "01101"
copy I 0
subI 1
bLTZA "01100"
discard
jumpA "01"

markA "01100"
discard
jumpA "00"
