
markM 1
cons 2
jumpI 14

markM 2
cons 3
jumpI 5

markM 3
cons 4
jumpI 32

markM 4
cons 10
outputCS
jumpI 0

markM 5
moveI 1
copyI 0
subI 1
bNEI 9
jumpI 13

markM 9
copyI 0

markM 10
subI 1
cons 12
jumpI 5

markM 12
cons 13
jumpI 44

markM 13
moveI 1
jump

markM 14
inputCS
copyI 0
subI 32
bNEI 18
bNEI 14

markM 18
cons 0
moveI 1

markM 19
subI 48
moveI 1
cons 10
cons 22
jumpI 44

markM 22
add
inputCS
copyI 0
subI 32
bNEI 27
jumpI 30

markM 27
copyI 0
subI 10
bNEI 29
jumpI 30

markM 29
copyI 0
subI -1
bNEI 19

markM 30
discard
moveI 1
jump

markM 32
cons 0
moveI 2
copyI 0
bNEI 35
cons 48
outputCS
bNE
jump

markM 35
copyI 0
bNEI 38
discard
jumpI 42

markM 38
cons 10
moddiv

markA "38"
subI -48
moveI 1
jumpI 35

markM 42
cons 43
jumpI 60

markM 43
jump

markM 44
moveI 2
moveI 2
cons 0
moveI 2
moveI 2

markM 48
copyI 0
bNEI 51
jumpI 58

markM 51
subI 1
moveI 2
copyI 2
add
moveI 2
moveI 2
jumpI 48

markM 58
bNES
moveI 1
jump

markM 60
moveI 1
copyI 0
bNEI 64
discard
jump

markM 64
outputCS
jumpI 60

markM 0
end
