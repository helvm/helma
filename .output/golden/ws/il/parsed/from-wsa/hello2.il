cons 0
cons 10
cons 33
cons 100
cons 108
cons 114
cons 111
cons 119
cons 32
cons 44
cons 111
cons 108
cons 108
cons 101
cons 72
callA "prints"
end

markA "prints"
copyI 0
bEZA "prints_end"
outputCS
jumpA "prints"

markA "prints_end"
discard
return

markA "printsln"
callA "prints"
cons 10
outputCS
return
