storeid 0 0

markA "block_0"
loadd 0
bNEA "branch_0_rl"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_rl"
loadd 0
subI 1
bNEA "branch_0_rr"
cons 10
jumpA "block_3"

markA "branch_0_rr"
loadd 0
subI 2
bNEA "branch_0_dl"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_dl"
loadd 0
subI 3
bNEA "branch_0_dr"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_dr"
loadd 0
subI 4
bNEA "branch_0_ll"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_ll"
loadd 0
subI 5
bNEA "branch_0_lr"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_lr"
loadd 0
subI 6
bNEA "branch_0_ul"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_ul"
loadd 0
subI 7
bNEA "branch_0_ur"
storeid 1 0
cons 10
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_2"
loadd 0
bNEA "branch_2_rl"
storeid 3 0
roll
jumpA "block_3"

markA "branch_2_rl"
loadd 0
subI 1
bNEA "branch_2_rr"
storeid 2 0
roll
jumpA "block_3"

markA "branch_2_rr"
loadd 0
subI 2
bNEA "branch_2_dl"
roll
jumpA "block_3"

markA "branch_2_dl"
loadd 0
subI 3
bNEA "branch_2_dr"
roll
jumpA "block_3"

markA "branch_2_dr"
loadd 0
subI 4
bNEA "branch_2_ll"
storeid 3 0
roll
jumpA "block_3"

markA "branch_2_ll"
loadd 0
subI 5
bNEA "branch_2_lr"
storeid 2 0
roll
jumpA "block_3"

markA "branch_2_lr"
loadd 0
subI 6
bNEA "branch_2_ul"
storeid 2 0
roll
jumpA "block_3"

markA "branch_2_ul"
loadd 0
subI 7
bNEA "branch_2_ur"
storeid 3 0
roll
jumpA "block_3"

markA "branch_2_ur"
end

markA "block_3"
loadd 0
bNEA "branch_3_rl"
storeid 4 0
discard
jumpA "block_0"

markA "branch_3_rl"
loadd 0
subI 1
bNEA "branch_3_rr"
storeid 5 0
discard
jumpA "block_0"

markA "branch_3_rr"
loadd 0
subI 2
bNEA "branch_3_dl"
storeid 5 0
discard
jumpA "block_0"

markA "branch_3_dl"
loadd 0
subI 3
bNEA "branch_3_dr"
storeid 4 0
discard
jumpA "block_0"

markA "branch_3_dr"
loadd 0
subI 4
bNEA "branch_3_ll"
discard
jumpA "block_0"

markA "branch_3_ll"
loadd 0
subI 5
bNEA "branch_3_lr"
discard
jumpA "block_0"

markA "branch_3_lr"
loadd 0
subI 6
bNEA "branch_3_ul"
lnot
jumpA "block_2"

markA "branch_3_ul"
loadd 0
subI 7
bNEA "branch_3_ur"
lnot
jumpA "block_2"

markA "branch_3_ur"
end
end
