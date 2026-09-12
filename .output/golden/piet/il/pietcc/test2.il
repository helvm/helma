storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
cons 12
jumpA "block_5"

markA "branch_0_rl"
loadsubDI 0 1
bNEA "branch_0_rr"
cons 12
jumpA "block_5"

markA "branch_0_rr"
loadsubDI 0 2
bNEA "branch_0_dl"
storeID 1 0
cons 12
jumpA "block_5"

markA "branch_0_dl"
loadsubDI 0 3
bNEA "branch_0_dr"
storeID 0 0
cons 12
jumpA "block_5"

markA "branch_0_dr"
loadsubDI 0 4
bNEA "branch_0_ll"
storeID 0 0
cons 12
jumpA "block_5"

markA "branch_0_ll"
loadsubDI 0 5
bNEA "branch_0_lr"
storeID 1 0
cons 12
jumpA "block_5"

markA "branch_0_lr"
loadsubDI 0 6
bNEA "branch_0_ul"
storeID 1 0
cons 12
jumpA "block_5"

markA "branch_0_ul"
loadsubDI 0 7
bNEA "branch_0_ur"
storeID 0 0
cons 12
jumpA "block_5"

markA "branch_0_ur"
end

markA "block_2"
end

markA "block_3"
end

markA "block_4"
loadD 0
bNEA "branch_4_rl"
storeID 3 0
copy I 0
jumpA "block_5"

markA "branch_4_rl"
loadsubDI 0 1
bNEA "branch_4_rr"
storeID 2 0
copy I 0
jumpA "block_5"

markA "branch_4_rr"
loadsubDI 0 2
bNEA "branch_4_dl"
copy I 0
jumpA "block_5"

markA "branch_4_dl"
loadsubDI 0 3
bNEA "branch_4_dr"
copy I 0
jumpA "block_5"

markA "branch_4_dr"
loadsubDI 0 4
bNEA "branch_4_ll"
storeID 7 0
outputCMaybeS
jumpA "block_3"

markA "branch_4_ll"
loadsubDI 0 5
bNEA "branch_4_lr"
storeID 6 0
outputCMaybeS
jumpA "block_3"

markA "branch_4_lr"
loadsubDI 0 6
bNEA "branch_4_ul"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ul"
loadsubDI 0 7
bNEA "branch_4_ur"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ur"
end

markA "block_5"
loadD 0
bNEA "branch_5_rl"
storeID 1 0
discard
jumpA "block_6"

markA "branch_5_rl"
loadsubDI 0 1
bNEA "branch_5_rr"
discard
jumpA "block_6"

markA "branch_5_rr"
loadsubDI 0 2
bNEA "branch_5_dl"
storeID 4 0
discard
jumpA "block_0"

markA "branch_5_dl"
loadsubDI 0 3
bNEA "branch_5_dr"
storeID 4 0
discard
jumpA "block_0"

markA "branch_5_dr"
loadsubDI 0 4
bNEA "branch_5_ll"
discard
jumpA "block_0"

markA "branch_5_ll"
loadsubDI 0 5
bNEA "branch_5_lr"
storeID 4 0
discard
jumpA "block_0"

markA "branch_5_lr"
loadsubDI 0 6
bNEA "branch_5_ul"
div
jumpA "block_4"

markA "branch_5_ul"
loadsubDI 0 7
bNEA "branch_5_ur"
div
jumpA "block_4"

markA "branch_5_ur"
end

markA "block_6"
loadD 0
bNEA "branch_6_rl"
copy I 0
jumpA "block_2"

markA "branch_6_rl"
loadsubDI 0 1
bNEA "branch_6_rr"
copy I 0
jumpA "block_2"

markA "branch_6_rr"
loadsubDI 0 2
bNEA "branch_6_dl"
storeID 5 0
cons 2
jumpA "block_5"

markA "branch_6_dl"
loadsubDI 0 3
bNEA "branch_6_dr"
storeID 4 0
cons 2
jumpA "block_5"

markA "branch_6_dr"
loadsubDI 0 4
bNEA "branch_6_ll"
cons 2
jumpA "block_5"

markA "branch_6_ll"
loadsubDI 0 5
bNEA "branch_6_lr"
cons 2
jumpA "block_5"

markA "branch_6_lr"
loadsubDI 0 6
bNEA "branch_6_ul"
storeID 7 0
copy I 0
jumpA "block_2"

markA "branch_6_ul"
loadsubDI 0 7
bNEA "branch_6_ur"
copy I 0
jumpA "block_2"

markA "branch_6_ur"
end
end
