
markM 1
move I 2
move I 2

markM 2
cons 0
move I 2
move I 2

markM 5
copy I 0
bNEI 8
jumpI 15

markM 8
subI 1
move I 2
copy I 2
add
move I 2
move I 2
jumpI 5

markM 15
bNES
move I 1
jump

markM 0
end
