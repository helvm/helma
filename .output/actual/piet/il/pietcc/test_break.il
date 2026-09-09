storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
storeID 3 0
lgt
jumpA "block_1"

markA "branch_0_rl"
loadD 0
subI 1
bNEA "branch_0_rr"
storeID 2 0
lgt
jumpA "block_1"

markA "branch_0_rr"
loadD 0
subI 2
bNEA "branch_0_dl"
lgt
jumpA "block_1"

markA "branch_0_dl"
loadD 0
subI 3
bNEA "branch_0_dr"
lgt
jumpA "block_1"

markA "branch_0_dr"
loadD 0
subI 4
bNEA "branch_0_ll"
storeID 3 0
lgt
jumpA "block_1"

markA "branch_0_ll"
loadD 0
subI 5
bNEA "branch_0_lr"
storeID 2 0
lgt
jumpA "block_1"

markA "branch_0_lr"
loadD 0
subI 6
bNEA "branch_0_ul"
storeID 2 0
lgt
jumpA "block_1"

markA "branch_0_ul"
loadD 0
subI 7
bNEA "branch_0_ur"
storeID 3 0
lgt
jumpA "block_1"

markA "branch_0_ur"
end

markA "block_1"
loadD 0
bNEA "branch_1_rl"
storeID 7 0
lgt
jumpA "block_0"

markA "branch_1_rl"
loadD 0
subI 1
bNEA "branch_1_rr"
storeID 6 0
lgt
jumpA "block_0"

markA "branch_1_rr"
loadD 0
subI 2
bNEA "branch_1_dl"
storeID 6 0
lgt
jumpA "block_0"

markA "branch_1_dl"
loadD 0
subI 3
bNEA "branch_1_dr"
storeID 7 0
lgt
jumpA "block_0"

markA "branch_1_dr"
loadD 0
subI 4
bNEA "branch_1_ll"
storeID 7 0
lgt
jumpA "block_0"

markA "branch_1_ll"
loadD 0
subI 5
bNEA "branch_1_lr"
storeID 6 0
lgt
jumpA "block_0"

markA "branch_1_lr"
loadD 0
subI 6
bNEA "branch_1_ul"
lgt
jumpA "block_0"

markA "branch_1_ul"
loadD 0
subI 7
bNEA "branch_1_ur"
lgt
jumpA "block_0"

markA "branch_1_ur"
end
end
