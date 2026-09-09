
markM 1
cons 0
move I 2

markM 2
copy I 0
bNEI 4
cons 48
outputCS
bNE
jump

markM 4
copy I 0
bNEI 7
discard
jumpI 11

markM 7
cons 10
moddiv

markA "7"
subI -48
move I 1

markM 10
jumpI 4

markM 11
cons 12
jumpI 13

markM 12
jump

markM 13
move I 1
copy I 0
bNEI 17
discard
jump

markM 17
outputCS
jumpI 13

markM 0
end
