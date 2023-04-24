
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
jumpI 44

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
jumpI 44

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
bNEI 35

markM 34
cons 48
outputCS
bNE
jump

markM 35
copyI 0

markM 36
bNEI 38

markM 37
sub
jumpI 42

markM 38
cons 10
moddiv

markA "38"

markM 39
subI -48

markM 40
moveI 1

markM 41
jumpI 35

markM 42
cons 43
jumpI 60

markM 43
jump

markM 44
moveI 2
moveI 2

markM 45
cons 0

markM 46
moveI 2

markM 47
moveI 2

markM 48
copyI 0

markM 49
bNEI 51

markM 50
jumpI 58

markM 51
subI 1

markM 52
moveI 2

markM 53
cons 0
copyI 3

markM 54
sub
sub

markM 55
moveI 2

markM 56
moveI 2

markM 57
jumpI 48

markM 58
bNES

markM 59
moveI 1
jump

markM 60
moveI 1

markM 61
copyI 0

markM 62
bNEI 64

markM 63
sub
jump

markM 64
outputCS

markM 65
jumpI 60

markM 66

markM 0
end
