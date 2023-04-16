
markM 1
jumpI 31

markM 2
moveI 1

markM 3
copyI 0
bNEI 6
discard
jump

markM 6
outputCS
jumpI 2

markM 8
cons 0
moveI 2
copyI 0
bNEI 11

markM 10
cons 48
outputCS
bNE
jump

markM 11
copyI 0
bNEI 14
discard
jumpI 18

markM 14
cons 10
moddiv

markA "14"
subI -48
moveI 1
jumpI 11

markM 18
cons 19
jumpI 2

markM 19
jump

markM 21
moveI 1
cons 23
jumpI 8

markM 23
cons 0
cons 114
cons 101
cons 101
cons 98
cons 32
cons 102
cons 111
cons 32
cons 115
cons 101
cons 108
cons 116
cons 116
cons 111
cons 98
cons 32
cons 25
jumpI 2

markM 25
jump

markM 26
moveI 1
cons 28
jumpI 21

markM 28
cons 0
cons 108
cons 108
cons 97
cons 119
cons 32
cons 101
cons 104
cons 116
cons 32
cons 110
cons 111
cons 32
cons 30
jumpI 2

markM 30
jump

markM 31
cons 3

markM 32
copyI 0
cons 33
jumpI 26

markM 33
cons 44
outputCS
cons 32
outputCS
copyI 0
cons 35
jumpI 21

markM 35
cons 10
outputCS
cons 0
cons 10
cons 100
cons 110
cons 117
cons 111
cons 114
cons 97
cons 32
cons 116
cons 105
cons 32
cons 115
cons 115
cons 97
cons 112
cons 32
cons 44
cons 110
cons 119
cons 111
cons 100
cons 32
cons 101
cons 110
cons 111
cons 32
cons 101
cons 107
cons 97
cons 84
cons 38
jumpI 2

markM 38
subI 1
copyI 0
cons 40
jumpI 26

markM 40
cons 10
cons 10
outputCS
outputCS
copyI 0
bNEI 32
bNEI 0

markM 0
end
