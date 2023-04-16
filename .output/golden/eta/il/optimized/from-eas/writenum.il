
markM 1
cons 0
moveI 2

markM 2
copyI 0
bNEI 4
cons 48
outputCS
bNE
jump

markM 4
copyI 0
bNEI 7
discard
jumpI 11

markM 7
cons 10
moddiv

markA "7"
subI -48
moveI 1

markM 10
jumpI 4

markM 11
cons 12
jumpI 13

markM 12
jump

markM 13
moveI 1
copyI 0
bNEI 17
discard
jump

markM 17
outputCS
jumpI 13

markM 0
end
