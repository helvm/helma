storeid 0 0
storeid 0 1
storeid 0 2
storeid 0 3
storeid 0 4
storeid 0 5
storeid 0 6
storeid 72 8
storeid 101 9
storeid 108 10
storeid 108 11
storeid 111 12
storeid 44 13
storeid 32 14
storeid 119 15
storeid 111 16
storeid 114 17
storeid 108 18
storeid 100 19
storeid 33 20
storeid 10 21
storeid 0 22
storeid 16 23

markA "00"
jumpA "01"

markA "01"
moved 5 3
cons 3
loadd 3
addI 16777215
addI 16777216
modI 16777216
store
loadd 3
addI 8
loadd 4
store
moved 3 5
moved 5 4
cons 5
loadd 5
subI 1
addI 16777216
modI 16777216
store
storeid 0 0
moved 5 1
storeid 0 0
moved 4 1
cons 1
loadd 1
addI 16777215
addI 16777216
modI 16777216
store
loadd 1
addI 8
loadd 0
store

markA "010"
moved 4 1
cons 1
loadd 1
addI 16777215
addI 16777216
modI 16777216
store
cons 0
loadd 1
addI 8
load
store
moved 0 1
cons 0
loadd 1
addI 8
load
store
loadd 0
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
moved 4 1
cons 1
loadd 1
addI 16777215
addI 16777216
modI 16777216
store
cons 0
loadd 1
addI 8
load
store
moved 0 1
cons 0
loadd 1
addI 8
load
store
moved 5 3
cons 3
loadd 3
addI 16777215
addI 16777216
modI 16777216
store
loadd 3
addI 8
loadd 0
store
moved 3 5
loadd 0
outputCS
cons 5
loadd 5
addI 1
addI 16777216
modI 16777216
store

markA "0110"
moved 4 1
cons 1
loadd 1
addI 16777215
addI 16777216
modI 16777216
store
cons 0
loadd 1
addI 8
load
store
moved 5 3
cons 3
loadd 3
addI 16777215
addI 16777216
modI 16777216
store
loadd 3
addI 8
loadd 0
store
moved 3 5
cons 0
loadd 0
addI 1
addI 16777216
modI 16777216
store
moved 4 1
cons 1
loadd 1
addI 16777215
addI 16777216
modI 16777216
store
loadd 1
addI 8
loadd 0
store
cons 0
loadd 5
addI 8
load
store
cons 5
loadd 5
addI 1
addI 16777216
modI 16777216
store
jumpA "010"

markA "0111"
storeid 0 0
moved 0 1
end
end

markA "01000"
copyI 0
subI 4
bLTZA "01111"
copyI 0
subI 6
bLTZA "010001"
copyI 0
subI 7
bLTZA "010010"
discard
jumpA "0111"

markA "010010"
discard
jumpA "0110"

markA "010001"
copyI 0
subI 5
bLTZA "010000"
discard
jumpA "0101"

markA "010000"
discard
jumpA "0100"

markA "01111"
copyI 0
subI 2
bLTZA "01101"
copyI 0
subI 3
bLTZA "01110"
discard
jumpA "011"

markA "01110"
discard
jumpA "010"

markA "01101"
copyI 0
subI 1
bLTZA "01100"
discard
jumpA "01"

markA "01100"
discard
jumpA "00"
