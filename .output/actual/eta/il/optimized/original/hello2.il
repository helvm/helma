
markM 1
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
cons 3
jumpI 4

markM 3
jumpI 0

markA "3"
bNEI 4
inputCS
subI 337

markM 4
moveI 1
copyI 0
bNEI 6
discard
jump

markM 6
outputCS
jumpI 4

markM 7

markM 0
end
