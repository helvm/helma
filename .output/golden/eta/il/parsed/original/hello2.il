
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

markM 2
cons 44
cons 111
cons 108
cons 108
cons 101
cons 72
cons 3
cons 1
cons 4
bNE

markM 3
cons 1
cons 0
bNE
cons 4
cons 22
bNE
inputCS
sub
bNE
outputCS
outputCS
moddiv

markA "3"
cons 4
bNE
inputCS
cons 337
sub

markM 4
cons 1
halibut
cons 0
halibut
cons 5
cons 0
cons 1
sub
sub
bNE

markM 5
sub
cons 11
cons 1
halibut
bNE

markM 6
outputCS
cons 7
cons 4
bNE
sub
sub
inputCS
cons 7
cons 0
jumpA "7"

markM 7

markM 0
end
