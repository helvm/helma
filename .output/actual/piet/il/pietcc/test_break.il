storeid 0 0

markA "block_0"
loadd 0
bNEA "branch_0_rl"
storeid 3 0
lgt
jumpA "block_1"

markA "branch_0_rl"
loadd 0
subI 1
bNEA "branch_0_rr"
storeid 2 0
lgt
jumpA "block_1"

markA "branch_0_rr"
loadd 0
subI 2
bNEA "branch_0_dl"
lgt
jumpA "block_1"

markA "branch_0_dl"
loadd 0
subI 3
bNEA "branch_0_dr"
lgt
jumpA "block_1"

markA "branch_0_dr"
loadd 0
subI 4
bNEA "branch_0_ll"
storeid 3 0
lgt
jumpA "block_1"

markA "branch_0_ll"
loadd 0
subI 5
bNEA "branch_0_lr"
storeid 2 0
lgt
jumpA "block_1"

markA "branch_0_lr"
loadd 0
subI 6
bNEA "branch_0_ul"
storeid 2 0
lgt
jumpA "block_1"

markA "branch_0_ul"
loadd 0
subI 7
bNEA "branch_0_ur"
storeid 3 0
lgt
jumpA "block_1"

markA "branch_0_ur"
end

markA "block_1"
loadd 0
bNEA "branch_1_rl"
storeid 7 0
lgt
jumpA "block_0"

markA "branch_1_rl"
loadd 0
subI 1
bNEA "branch_1_rr"
storeid 6 0
lgt
jumpA "block_0"

markA "branch_1_rr"
loadd 0
subI 2
bNEA "branch_1_dl"
storeid 6 0
lgt
jumpA "block_0"

markA "branch_1_dl"
loadd 0
subI 3
bNEA "branch_1_dr"
storeid 7 0
lgt
jumpA "block_0"

markA "branch_1_dr"
loadd 0
subI 4
bNEA "branch_1_ll"
storeid 7 0
lgt
jumpA "block_0"

markA "branch_1_ll"
loadd 0
subI 5
bNEA "branch_1_lr"
storeid 6 0
lgt
jumpA "block_0"

markA "branch_1_lr"
loadd 0
subI 6
bNEA "branch_1_ul"
lgt
jumpA "block_0"

markA "branch_1_ul"
loadd 0
subI 7
bNEA "branch_1_ur"
lgt
jumpA "block_0"

markA "branch_1_ur"
end
end
