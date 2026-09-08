storeid 0 0

markA "block_0"
loadd 0
bNEA "branch_0_rl"
cons 12
jumpA "block_5"

markA "branch_0_rl"
loadd 0
subI 1
bNEA "branch_0_rr"
cons 12
jumpA "block_5"

markA "branch_0_rr"
loadd 0
subI 2
bNEA "branch_0_dl"
storeid 1 0
cons 12
jumpA "block_5"

markA "branch_0_dl"
loadd 0
subI 3
bNEA "branch_0_dr"
storeid 0 0
cons 12
jumpA "block_5"

markA "branch_0_dr"
loadd 0
subI 4
bNEA "branch_0_ll"
storeid 0 0
cons 12
jumpA "block_5"

markA "branch_0_ll"
loadd 0
subI 5
bNEA "branch_0_lr"
storeid 1 0
cons 12
jumpA "block_5"

markA "branch_0_lr"
loadd 0
subI 6
bNEA "branch_0_ul"
storeid 1 0
cons 12
jumpA "block_5"

markA "branch_0_ul"
loadd 0
subI 7
bNEA "branch_0_ur"
storeid 0 0
cons 12
jumpA "block_5"

markA "branch_0_ur"
end

markA "block_2"
end

markA "block_3"
end

markA "block_4"
loadd 0
bNEA "branch_4_rl"
storeid 3 0
copyI 0
jumpA "block_5"

markA "branch_4_rl"
loadd 0
subI 1
bNEA "branch_4_rr"
storeid 2 0
copyI 0
jumpA "block_5"

markA "branch_4_rr"
loadd 0
subI 2
bNEA "branch_4_dl"
copyI 0
jumpA "block_5"

markA "branch_4_dl"
loadd 0
subI 3
bNEA "branch_4_dr"
copyI 0
jumpA "block_5"

markA "branch_4_dr"
loadd 0
subI 4
bNEA "branch_4_ll"
storeid 7 0
outputCMaybeS
jumpA "block_3"

markA "branch_4_ll"
loadd 0
subI 5
bNEA "branch_4_lr"
storeid 6 0
outputCMaybeS
jumpA "block_3"

markA "branch_4_lr"
loadd 0
subI 6
bNEA "branch_4_ul"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ul"
loadd 0
subI 7
bNEA "branch_4_ur"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ur"
end

markA "block_5"
loadd 0
bNEA "branch_5_rl"
storeid 1 0
discard
jumpA "block_6"

markA "branch_5_rl"
loadd 0
subI 1
bNEA "branch_5_rr"
discard
jumpA "block_6"

markA "branch_5_rr"
loadd 0
subI 2
bNEA "branch_5_dl"
storeid 4 0
discard
jumpA "block_0"

markA "branch_5_dl"
loadd 0
subI 3
bNEA "branch_5_dr"
storeid 4 0
discard
jumpA "block_0"

markA "branch_5_dr"
loadd 0
subI 4
bNEA "branch_5_ll"
discard
jumpA "block_0"

markA "branch_5_ll"
loadd 0
subI 5
bNEA "branch_5_lr"
storeid 4 0
discard
jumpA "block_0"

markA "branch_5_lr"
loadd 0
subI 6
bNEA "branch_5_ul"
div
jumpA "block_4"

markA "branch_5_ul"
loadd 0
subI 7
bNEA "branch_5_ur"
div
jumpA "block_4"

markA "branch_5_ur"
end

markA "block_6"
loadd 0
bNEA "branch_6_rl"
copyI 0
jumpA "block_2"

markA "branch_6_rl"
loadd 0
subI 1
bNEA "branch_6_rr"
copyI 0
jumpA "block_2"

markA "branch_6_rr"
loadd 0
subI 2
bNEA "branch_6_dl"
storeid 5 0
cons 2
jumpA "block_5"

markA "branch_6_dl"
loadd 0
subI 3
bNEA "branch_6_dr"
storeid 4 0
cons 2
jumpA "block_5"

markA "branch_6_dr"
loadd 0
subI 4
bNEA "branch_6_ll"
cons 2
jumpA "block_5"

markA "branch_6_ll"
loadd 0
subI 5
bNEA "branch_6_lr"
cons 2
jumpA "block_5"

markA "branch_6_lr"
loadd 0
subI 6
bNEA "branch_6_ul"
storeid 7 0
copyI 0
jumpA "block_2"

markA "branch_6_ul"
loadd 0
subI 7
bNEA "branch_6_ur"
copyI 0
jumpA "block_2"

markA "branch_6_ur"
end
end
