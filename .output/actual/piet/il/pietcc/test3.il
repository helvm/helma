storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_rl"
loadsubDI 0 1
bNEA "branch_0_rr"
cons 10
jumpA "block_3"

markA "branch_0_rr"
loadsubDI 0 2
bNEA "branch_0_dl"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_dl"
loadsubDI 0 3
bNEA "branch_0_dr"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_dr"
loadsubDI 0 4
bNEA "branch_0_ll"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_ll"
loadsubDI 0 5
bNEA "branch_0_lr"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_lr"
loadsubDI 0 6
bNEA "branch_0_ul"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_ul"
loadsubDI 0 7
bNEA "branch_0_ur"
storeID 1 0
cons 10
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_2"
loadD 0
bNEA "branch_2_rl"
storeID 3 0
roll
jumpA "block_3"

markA "branch_2_rl"
loadsubDI 0 1
bNEA "branch_2_rr"
storeID 2 0
roll
jumpA "block_3"

markA "branch_2_rr"
loadsubDI 0 2
bNEA "branch_2_dl"
roll
jumpA "block_3"

markA "branch_2_dl"
loadsubDI 0 3
bNEA "branch_2_dr"
roll
jumpA "block_3"

markA "branch_2_dr"
loadsubDI 0 4
bNEA "branch_2_ll"
storeID 3 0
roll
jumpA "block_3"

markA "branch_2_ll"
loadsubDI 0 5
bNEA "branch_2_lr"
storeID 2 0
roll
jumpA "block_3"

markA "branch_2_lr"
loadsubDI 0 6
bNEA "branch_2_ul"
storeID 2 0
roll
jumpA "block_3"

markA "branch_2_ul"
loadsubDI 0 7
bNEA "branch_2_ur"
storeID 3 0
roll
jumpA "block_3"

markA "branch_2_ur"
end

markA "block_3"
loadD 0
bNEA "branch_3_rl"
storeID 4 0
discard
jumpA "block_0"

markA "branch_3_rl"
loadsubDI 0 1
bNEA "branch_3_rr"
storeID 5 0
discard
jumpA "block_0"

markA "branch_3_rr"
loadsubDI 0 2
bNEA "branch_3_dl"
storeID 5 0
discard
jumpA "block_0"

markA "branch_3_dl"
loadsubDI 0 3
bNEA "branch_3_dr"
storeID 4 0
discard
jumpA "block_0"

markA "branch_3_dr"
loadsubDI 0 4
bNEA "branch_3_ll"
discard
jumpA "block_0"

markA "branch_3_ll"
loadsubDI 0 5
bNEA "branch_3_lr"
discard
jumpA "block_0"

markA "branch_3_lr"
loadsubDI 0 6
bNEA "branch_3_ul"
lnot
jumpA "block_2"

markA "branch_3_ul"
loadsubDI 0 7
bNEA "branch_3_ur"
lnot
jumpA "block_2"

markA "branch_3_ur"
end
end
