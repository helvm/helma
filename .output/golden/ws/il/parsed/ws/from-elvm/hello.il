cons 0
cons 0
store
cons 1
cons 0
store
cons 2
cons 0
store
cons 3
cons 0
store
cons 4
cons 0
store
cons 5
cons 0
store
cons 6
cons 0
store
cons 8
cons 72
store
cons 9
cons 101
store
cons 10
cons 108
store
cons 11
cons 108
store
cons 12
cons 111
store
cons 13
cons 44
store
cons 14
cons 32
store
cons 15
cons 119
store
cons 16
cons 111
store
cons 17
cons 114
store
cons 18
cons 108
store
cons 19
cons 100
store
cons 20
cons 33
store
cons 21
cons 10
store
cons 22
cons 0
store
cons 23
cons 16
store

markA "00"
jumpA "01"

markA "01"
cons 3
cons 5
load
store
cons 3
cons 3
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 3
load
cons 8
add
cons 4
load
store
cons 5
cons 3
load
store
cons 4
cons 5
load
store
cons 5
cons 5
load
cons 1
sub
cons 16777216
add
cons 16777216
mod
store
cons 0
cons 0
store
cons 1
cons 5
load
store
cons 0
cons 0
store
cons 1
cons 4
load
store
cons 1
cons 1
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 1
load
cons 8
add
cons 0
load
store

markA "010"
cons 1
cons 4
load
store
cons 1
cons 1
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 0
cons 1
load
cons 8
add
load
store
cons 1
cons 0
load
store
cons 0
cons 1
load
cons 8
add
load
store
cons 0
load
cons 0
sub
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
cons 1
cons 4
load
store
cons 1
cons 1
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 0
cons 1
load
cons 8
add
load
store
cons 1
cons 0
load
store
cons 0
cons 1
load
cons 8
add
load
store
cons 3
cons 5
load
store
cons 3
cons 3
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 3
load
cons 8
add
cons 0
load
store
cons 5
cons 3
load
store
cons 0
load
outputCS
cons 5
cons 5
load
cons 1
add
cons 16777216
add
cons 16777216
mod
store

markA "0110"
cons 1
cons 4
load
store
cons 1
cons 1
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 0
cons 1
load
cons 8
add
load
store
cons 3
cons 5
load
store
cons 3
cons 3
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 3
load
cons 8
add
cons 0
load
store
cons 5
cons 3
load
store
cons 0
cons 0
load
cons 1
add
cons 16777216
add
cons 16777216
mod
store
cons 1
cons 4
load
store
cons 1
cons 1
load
cons 16777215
add
cons 16777216
add
cons 16777216
mod
store
cons 1
load
cons 8
add
cons 0
load
store
cons 0
cons 5
load
cons 8
add
load
store
cons 5
cons 5
load
cons 1
add
cons 16777216
add
cons 16777216
mod
store
jumpA "010"

markA "0111"
cons 0
cons 0
store
cons 1
cons 0
load
store
end
end

markA "01000"
copyI 0
cons 4
sub
bLTZA "01111"
copyI 0
cons 6
sub
bLTZA "010001"
copyI 0
cons 7
sub
bLTZA "010010"
discard
jumpA "0111"

markA "010010"
discard
jumpA "0110"

markA "010001"
copyI 0
cons 5
sub
bLTZA "010000"
discard
jumpA "0101"

markA "010000"
discard
jumpA "0100"

markA "01111"
copyI 0
cons 2
sub
bLTZA "01101"
copyI 0
cons 3
sub
bLTZA "01110"
discard
jumpA "011"

markA "01110"
discard
jumpA "010"

markA "01101"
copyI 0
cons 1
sub
bLTZA "01100"
discard
jumpA "01"

markA "01100"
discard
jumpA "00"
