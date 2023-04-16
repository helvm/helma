
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
jumpI 42

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
jumpI 42

markA "21"
cons 22
moddiv
bNE
inputCS
bNE
moddiv

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

markM 33
copyI 0
bNEI 36
discard
jumpI 40

markM 36
cons 10
moddiv

markA "36"
subI -48
moveI 1
jumpI 33

markM 40
cons 41
jumpI 58

markM 41
jump

markM 42
moveI 2
moveI 2
cons 0
moveI 2
moveI 2

markM 46
copyI 0
bNEI 49

markM 48
jumpI 56

markM 49
subI 1
moveI 2
copyI 2
add
moveI 2
moveI 2
jumpI 46

markM 56
bNES
moveI 1
jump

markM 58
moveI 1
copyI 0
bNEI 62
discard
jump

markM 62
outputCS
jumpI 58

markM 0
end
