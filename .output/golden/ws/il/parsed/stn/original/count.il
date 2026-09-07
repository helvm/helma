cons 1

markA "C"
copyI 0
outputDS
cons 10
outputCS
cons 1
add
copyI 0
cons 11
sub
bEZA "E"
jumpA "C"

markA "E"
discard
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
cons 1
add
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
cons 10
sub
bEZA "read_end"
discard
cons 1
add
jumpA "read"

markA "read_end"
discard
cons 1
add
cons 0
store
return

markA "newline"
cons 10
cons 13
outputCS
outputCS
return
