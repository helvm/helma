storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
cons 6
jumpA "block_1"

markA "branch_0_rl"
loadD 0
subI 1
bNEA "branch_0_rr"
cons 6
jumpA "block_1"

markA "branch_0_rr"
loadD 0
subI 2
bNEA "branch_0_dl"
storeID 1 0
cons 6
jumpA "block_1"

markA "branch_0_dl"
loadD 0
subI 3
bNEA "branch_0_dr"
storeID 0 0
cons 6
jumpA "block_1"

markA "branch_0_dr"
loadD 0
subI 4
bNEA "branch_0_ll"
storeID 0 0
cons 6
jumpA "block_1"

markA "branch_0_ll"
loadD 0
subI 5
bNEA "branch_0_lr"
storeID 1 0
cons 6
jumpA "block_1"

markA "branch_0_lr"
loadD 0
subI 6
bNEA "branch_0_ul"
storeID 1 0
cons 6
jumpA "block_1"

markA "branch_0_ul"
loadD 0
subI 7
bNEA "branch_0_ur"
storeID 0 0
cons 6
jumpA "block_1"

markA "branch_0_ur"
end

markA "block_1"
loadD 0
bNEA "branch_1_rl"
storeID 4 0
discard
jumpA "block_0"

markA "branch_1_rl"
loadD 0
subI 1
bNEA "branch_1_rr"
storeID 5 0
discard
jumpA "block_0"

markA "branch_1_rr"
loadD 0
subI 2
bNEA "branch_1_dl"
storeID 5 0
discard
jumpA "block_0"

markA "branch_1_dl"
loadD 0
subI 3
bNEA "branch_1_dr"
storeID 4 0
discard
jumpA "block_0"

markA "branch_1_dr"
loadD 0
subI 4
bNEA "branch_1_ll"
discard
jumpA "block_0"

markA "branch_1_ll"
loadD 0
subI 5
bNEA "branch_1_lr"
discard
jumpA "block_0"

markA "branch_1_lr"
loadD 0
subI 6
bNEA "branch_1_ul"
storeID 5 0
discard
jumpA "block_0"

markA "branch_1_ul"
loadD 0
subI 7
bNEA "branch_1_ur"
storeID 4 0
discard
jumpA "block_0"

markA "branch_1_ur"
end
end
