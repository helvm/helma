
markM 1
cons 0
moveI 2

markM 2
copyI 0
bNEI 4

markM 3
cons 48
outputCS
bNE
jump

markM 4
copyI 0

markM 5
bNEI 7

markM 6
sub
jumpI 11

markM 7
cons 10
moddiv

markA "7"

markM 8
subI -48

markM 9
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

markM 14
copyI 0

markM 15
bNEI 17

markM 16
sub
jump

markM 17
outputCS

markM 18
jumpI 13

markM 19

markM 0
end
