
markM 1
inputCS

markM 2
copyI 0
subI 32
bNEI 5
bNEI 1

markM 5
cons 0
moveI 1

markM 6
subI 48
moveI 1
cons 10
cons 9
jumpI 19

markM 9
cons 0
moveI 1

markM 10
sub
sub
inputCS
copyI 0
subI 32
bNEI 14
jumpI 17

markM 14
copyI 0
subI 10
bNEI 16
jumpI 17

markM 16
copyI 0
subI -1
bNEI 6

markM 17
discard
moveI 1
jump

markM 19
moveI 2
moveI 2
cons 0
moveI 2
moveI 2

markM 23
copyI 0
bNEI 26
jumpI 33

markM 26
subI 1
moveI 2
copyI 2
add
moveI 2
moveI 2

markM 32
jumpI 23

markM 33
bNES
moveI 1
jump

markM 0
end
