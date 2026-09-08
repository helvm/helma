storeid 0 0

markA "block_0"
loadd 0
bNEA "branch_0_rl"
cons 6
jumpA "block_1"

markA "branch_0_rl"
loadd 0
subI 1
bNEA "branch_0_rr"
cons 6
jumpA "block_1"

markA "branch_0_rr"
loadd 0
subI 2
bNEA "branch_0_dl"
storeid 1 0
cons 6
jumpA "block_1"

markA "branch_0_dl"
loadd 0
subI 3
bNEA "branch_0_dr"
storeid 0 0
cons 6
jumpA "block_1"

markA "branch_0_dr"
loadd 0
subI 4
bNEA "branch_0_ll"
storeid 0 0
cons 6
jumpA "block_1"

markA "branch_0_ll"
loadd 0
subI 5
bNEA "branch_0_lr"
storeid 1 0
cons 6
jumpA "block_1"

markA "branch_0_lr"
loadd 0
subI 6
bNEA "branch_0_ul"
storeid 1 0
cons 6
jumpA "block_1"

markA "branch_0_ul"
loadd 0
subI 7
bNEA "branch_0_ur"
storeid 0 0
cons 6
jumpA "block_1"

markA "branch_0_ur"
end

markA "block_1"
loadd 0
bNEA "branch_1_rl"
storeid 4 0
discard
jumpA "block_0"

markA "branch_1_rl"
loadd 0
subI 1
bNEA "branch_1_rr"
storeid 5 0
discard
jumpA "block_0"

markA "branch_1_rr"
loadd 0
subI 2
bNEA "branch_1_dl"
storeid 5 0
discard
jumpA "block_0"

markA "branch_1_dl"
loadd 0
subI 3
bNEA "branch_1_dr"
storeid 4 0
discard
jumpA "block_0"

markA "branch_1_dr"
loadd 0
subI 4
bNEA "branch_1_ll"
discard
jumpA "block_0"

markA "branch_1_ll"
loadd 0
subI 5
bNEA "branch_1_lr"
discard
jumpA "block_0"

markA "branch_1_lr"
loadd 0
subI 6
bNEA "branch_1_ul"
storeid 5 0
discard
jumpA "block_0"

markA "branch_1_ul"
loadd 0
subI 7
bNEA "branch_1_ur"
storeid 4 0
discard
jumpA "block_0"

markA "branch_1_ur"
end
end
