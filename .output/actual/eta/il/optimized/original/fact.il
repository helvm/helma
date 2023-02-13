
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

markM 6
copyI 0

markM 7
subI 1
bNEI 9

markM 8
jumpI 13

markM 9
copyI 0

markM 10
subI 1

markM 11
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

markM 15
copyI 0
subI 32

markM 16
bNEI 18

markM 17
bNEI 14

markM 18
cons 0
moveI 1

markM 19
subI 48

markM 20
moveI 1

markM 21
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
cons 0
moveI 1

markM 23
sub
sub

markM 24
inputCS

markM 25
copyI 0
subI 32
bNEI 27

markM 26
jumpI 30

markM 27
copyI 0
subI 10
bNEI 29

markM 28
jumpI 30

markM 29
copyI 0
subI -1
bNEI 19

markM 30
discard

markM 31
moveI 1
jump

markM 32
cons 0
moveI 2

markM 33
copyI 0

markM 34
bNEI 36

markM 35
sub
jumpI 40

markM 36
cons 10
moddiv

markA "36"

markM 37
subI -48

markM 38
moveI 1

markM 39
jumpI 33

markM 40
cons 41
jumpI 58

markM 41
jump

markM 42
moveI 2
moveI 2

markM 43
cons 0

markM 44
moveI 2

markM 45
moveI 2

markM 46
copyI 0

markM 47
bNEI 49

markM 48
jumpI 56

markM 49
subI 1

markM 50
moveI 2

markM 51
cons 0
copyI 3

markM 52
sub
sub

markM 53
moveI 2

markM 54
moveI 2

markM 55
jumpI 46

markM 56
bNES

markM 57
moveI 1
jump

markM 58
moveI 1

markM 59
copyI 0

markM 60
bNEI 62

markM 61
sub
jump

markM 62
outputCS

markM 63
jumpI 58

markM 64

markM 0
end
