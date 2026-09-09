storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_rl"
loadsubDI 0 1
bNEA "branch_0_rr"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_rr"
loadsubDI 0 2
bNEA "branch_0_dl"
inputDS
jumpA "block_3"

markA "branch_0_dl"
loadsubDI 0 3
bNEA "branch_0_dr"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_dr"
loadsubDI 0 4
bNEA "branch_0_ll"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_ll"
loadsubDI 0 5
bNEA "branch_0_lr"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_lr"
loadsubDI 0 6
bNEA "branch_0_ul"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_ul"
loadsubDI 0 7
bNEA "branch_0_ur"
storeID 2 0
inputDS
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_3"
loadD 0
bNEA "branch_3_rl"
storeID 3 0
inputDS
jumpA "block_5"

markA "branch_3_rl"
loadsubDI 0 1
bNEA "branch_3_rr"
storeID 2 0
inputDS
jumpA "block_5"

markA "branch_3_rr"
loadsubDI 0 2
bNEA "branch_3_dl"
inputDS
jumpA "block_5"

markA "branch_3_dl"
loadsubDI 0 3
bNEA "branch_3_dr"
inputDS
jumpA "block_5"

markA "branch_3_dr"
loadsubDI 0 4
bNEA "branch_3_ll"
storeID 7 0
mod
jumpA "block_0"

markA "branch_3_ll"
loadsubDI 0 5
bNEA "branch_3_lr"
storeID 6 0
mod
jumpA "block_0"

markA "branch_3_lr"
loadsubDI 0 6
bNEA "branch_3_ul"
mod
jumpA "block_0"

markA "branch_3_ul"
loadsubDI 0 7
bNEA "branch_3_ur"
mod
jumpA "block_0"

markA "branch_3_ur"
end

markA "block_4"
loadD 0
bNEA "branch_4_rl"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_rl"
loadsubDI 0 1
bNEA "branch_4_rr"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_rr"
loadsubDI 0 2
bNEA "branch_4_dl"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_dl"
loadsubDI 0 3
bNEA "branch_4_dr"
cons 4
jumpA "block_11"

markA "branch_4_dr"
loadsubDI 0 4
bNEA "branch_4_ll"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_ll"
loadsubDI 0 5
bNEA "branch_4_lr"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_lr"
loadsubDI 0 6
bNEA "branch_4_ul"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_ul"
loadsubDI 0 7
bNEA "branch_4_ur"
storeID 3 0
cons 4
jumpA "block_11"

markA "branch_4_ur"
end

markA "block_5"
loadD 0
bNEA "branch_5_rl"
storeID 3 0
copy I 0
jumpA "block_6"

markA "branch_5_rl"
loadsubDI 0 1
bNEA "branch_5_rr"
storeID 2 0
copy I 0
jumpA "block_6"

markA "branch_5_rr"
loadsubDI 0 2
bNEA "branch_5_dl"
copy I 0
jumpA "block_6"

markA "branch_5_dl"
loadsubDI 0 3
bNEA "branch_5_dr"
copy I 0
jumpA "block_6"

markA "branch_5_dr"
loadsubDI 0 4
bNEA "branch_5_ll"
storeID 7 0
mod
jumpA "block_3"

markA "branch_5_ll"
loadsubDI 0 5
bNEA "branch_5_lr"
storeID 6 0
mod
jumpA "block_3"

markA "branch_5_lr"
loadsubDI 0 6
bNEA "branch_5_ul"
mod
jumpA "block_3"

markA "branch_5_ul"
loadsubDI 0 7
bNEA "branch_5_ur"
mod
jumpA "block_3"

markA "branch_5_ur"
end

markA "block_6"
loadD 0
bNEA "branch_6_rl"
jumpA "block_8"

markA "branch_6_rl"
loadsubDI 0 1
bNEA "branch_6_rr"
jumpA "block_8"

markA "branch_6_rr"
loadsubDI 0 2
bNEA "branch_6_dl"
storeID 3 0
cons 3
jumpA "block_4"

markA "branch_6_dl"
loadsubDI 0 3
bNEA "branch_6_dr"
cons 3
jumpA "block_4"

markA "branch_6_dr"
loadsubDI 0 4
bNEA "branch_6_ll"
cons 3
jumpA "block_4"

markA "branch_6_ll"
loadsubDI 0 5
bNEA "branch_6_lr"
cons 3
jumpA "block_4"

markA "branch_6_lr"
loadsubDI 0 6
bNEA "branch_6_ul"
storeID 1 0
jumpA "block_8"

markA "branch_6_ul"
loadsubDI 0 7
bNEA "branch_6_ur"
storeID 0 0
jumpA "block_8"

markA "branch_6_ur"
end

markA "block_8"
loadD 0
bNEA "branch_8_rl"
div
jumpA "block_9"

markA "branch_8_rl"
loadsubDI 0 1
bNEA "branch_8_rr"
div
jumpA "block_9"

markA "branch_8_rr"
loadsubDI 0 2
bNEA "branch_8_dl"
storeID 5 0
jumpA "block_6"

markA "branch_8_dl"
loadsubDI 0 3
bNEA "branch_8_dr"
storeID 4 0
jumpA "block_6"

markA "branch_8_dr"
loadsubDI 0 4
bNEA "branch_8_ll"
jumpA "block_6"

markA "branch_8_ll"
loadsubDI 0 5
bNEA "branch_8_lr"
jumpA "block_6"

markA "branch_8_lr"
loadsubDI 0 6
bNEA "branch_8_ul"
storeID 1 0
div
jumpA "block_9"

markA "branch_8_ul"
loadsubDI 0 7
bNEA "branch_8_ur"
storeID 0 0
div
jumpA "block_9"

markA "branch_8_ur"
end

markA "block_9"
loadD 0
bNEA "branch_9_rl"
storeID 4 0
copy I 0
jumpA "block_8"

markA "branch_9_rl"
loadsubDI 0 1
bNEA "branch_9_rr"
storeID 5 0
copy I 0
jumpA "block_8"

markA "branch_9_rr"
loadsubDI 0 2
bNEA "branch_9_dl"
storeID 5 0
copy I 0
jumpA "block_8"

markA "branch_9_dl"
loadsubDI 0 3
bNEA "branch_9_dr"
storeID 4 0
copy I 0
jumpA "block_8"

markA "branch_9_dr"
loadsubDI 0 4
bNEA "branch_9_ll"
copy I 0
jumpA "block_8"

markA "branch_9_ll"
loadsubDI 0 5
bNEA "branch_9_lr"
copy I 0
jumpA "block_8"

markA "branch_9_lr"
loadsubDI 0 6
bNEA "branch_9_ul"
storeID 5 0
copy I 0
jumpA "block_8"

markA "branch_9_ul"
loadsubDI 0 7
bNEA "branch_9_ur"
storeID 4 0
copy I 0
jumpA "block_8"

markA "branch_9_ur"
end

markA "block_11"
loadD 0
bNEA "branch_11_rl"
storeID 3 0
roll
jumpA "block_14"

markA "branch_11_rl"
loadsubDI 0 1
bNEA "branch_11_rr"
storeID 2 0
roll
jumpA "block_14"

markA "branch_11_rr"
loadsubDI 0 2
bNEA "branch_11_dl"
roll
jumpA "block_14"

markA "branch_11_dl"
loadsubDI 0 3
bNEA "branch_11_dr"
roll
jumpA "block_14"

markA "branch_11_dr"
loadsubDI 0 4
bNEA "branch_11_ll"
storeID 7 0
discard
jumpA "block_4"

markA "branch_11_ll"
loadsubDI 0 5
bNEA "branch_11_lr"
storeID 6 0
discard
jumpA "block_4"

markA "branch_11_lr"
loadsubDI 0 6
bNEA "branch_11_ul"
discard
jumpA "block_4"

markA "branch_11_ul"
loadsubDI 0 7
bNEA "branch_11_ur"
discard
jumpA "block_4"

markA "branch_11_ur"
end

markA "block_12"
loadD 0
bNEA "branch_12_rl"
jumpA "block_9"

markA "branch_12_rl"
loadsubDI 0 1
bNEA "branch_12_rr"
discard
jumpA "block_23"

markA "branch_12_rr"
loadsubDI 0 2
bNEA "branch_12_dl"
storeID 1 0
discard
jumpA "block_23"

markA "branch_12_dl"
loadsubDI 0 3
bNEA "branch_12_dr"
storeID 0 0
jumpA "block_9"

markA "branch_12_dr"
loadsubDI 0 4
bNEA "branch_12_ll"
storeID 0 0
jumpA "block_9"

markA "branch_12_ll"
loadsubDI 0 5
bNEA "branch_12_lr"
storeID 1 0
discard
jumpA "block_23"

markA "branch_12_lr"
loadsubDI 0 6
bNEA "branch_12_ul"
storeID 1 0
discard
jumpA "block_23"

markA "branch_12_ul"
loadsubDI 0 7
bNEA "branch_12_ur"
storeID 0 0
jumpA "block_9"

markA "branch_12_ur"
end

markA "block_14"
loadD 0
bNEA "branch_14_rl"
storeID 2 0
mod
jumpA "block_19"

markA "branch_14_rl"
loadsubDI 0 1
bNEA "branch_14_rr"
storeID 2 0
mod
jumpA "block_19"

markA "branch_14_rr"
loadsubDI 0 2
bNEA "branch_14_dl"
mod
jumpA "block_19"

markA "branch_14_dl"
loadsubDI 0 3
bNEA "branch_14_dr"
storeID 2 0
mod
jumpA "block_19"

markA "branch_14_dr"
loadsubDI 0 4
bNEA "branch_14_ll"
storeID 6 0
lnot
jumpA "block_11"

markA "branch_14_ll"
loadsubDI 0 5
bNEA "branch_14_lr"
storeID 6 0
lnot
jumpA "block_11"

markA "branch_14_lr"
loadsubDI 0 6
bNEA "branch_14_ul"
lnot
jumpA "block_11"

markA "branch_14_ul"
loadsubDI 0 7
bNEA "branch_14_ur"
storeID 6 0
lnot
jumpA "block_11"

markA "branch_14_ur"
end

markA "block_15"
loadD 0
bNEA "branch_15_rl"
lnot
jumpA "block_16"

markA "branch_15_rl"
loadsubDI 0 1
bNEA "branch_15_rr"
storeID 0 0
lnot
jumpA "block_16"

markA "branch_15_rr"
loadsubDI 0 2
bNEA "branch_15_dl"
storeID 4 0
lgt
jumpA "block_24"

markA "branch_15_dl"
loadsubDI 0 3
bNEA "branch_15_dr"
storeID 4 0
lgt
jumpA "block_24"

markA "branch_15_dr"
loadsubDI 0 4
bNEA "branch_15_ll"
lgt
jumpA "block_24"

markA "branch_15_ll"
loadsubDI 0 5
bNEA "branch_15_lr"
storeID 4 0
lgt
jumpA "block_24"

markA "branch_15_lr"
loadsubDI 0 6
bNEA "branch_15_ul"
storeID 0 0
lnot
jumpA "block_16"

markA "branch_15_ul"
loadsubDI 0 7
bNEA "branch_15_ur"
storeID 0 0
lnot
jumpA "block_16"

markA "branch_15_ur"
end

markA "block_16"
loadD 0
bNEA "branch_16_rl"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_12"

markA "branch_16_rl"
loadsubDI 0 1
bNEA "branch_16_rr"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_12"

markA "branch_16_rr"
loadsubDI 0 2
bNEA "branch_16_dl"
storeID 5 0
roll
jumpA "block_15"

markA "branch_16_dl"
loadsubDI 0 3
bNEA "branch_16_dr"
storeID 4 0
roll
jumpA "block_15"

markA "branch_16_dr"
loadsubDI 0 4
bNEA "branch_16_ll"
roll
jumpA "block_15"

markA "branch_16_ll"
loadsubDI 0 5
bNEA "branch_16_lr"
roll
jumpA "block_15"

markA "branch_16_lr"
loadsubDI 0 6
bNEA "branch_16_ul"
storeID 1 0
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_12"

markA "branch_16_ul"
loadsubDI 0 7
bNEA "branch_16_ur"
storeID 0 0
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_12"

markA "branch_16_ur"
end

markA "block_19"
loadD 0
bNEA "branch_19_rl"
storeID 3 0
copy I 0
jumpA "block_22"

markA "branch_19_rl"
loadsubDI 0 1
bNEA "branch_19_rr"
storeID 2 0
copy I 0
jumpA "block_22"

markA "branch_19_rr"
loadsubDI 0 2
bNEA "branch_19_dl"
copy I 0
jumpA "block_22"

markA "branch_19_dl"
loadsubDI 0 3
bNEA "branch_19_dr"
copy I 0
jumpA "block_22"

markA "branch_19_dr"
loadsubDI 0 4
bNEA "branch_19_ll"
storeID 7 0
inputDS
jumpA "block_14"

markA "branch_19_ll"
loadsubDI 0 5
bNEA "branch_19_lr"
storeID 6 0
inputDS
jumpA "block_14"

markA "branch_19_lr"
loadsubDI 0 6
bNEA "branch_19_ul"
inputDS
jumpA "block_14"

markA "branch_19_ul"
loadsubDI 0 7
bNEA "branch_19_ur"
inputDS
jumpA "block_14"

markA "branch_19_ur"
end

markA "block_21"
end

markA "block_22"
loadD 0
bNEA "branch_22_rl"
storeID 3 0
cons 1
jumpA "block_24"

markA "branch_22_rl"
loadsubDI 0 1
bNEA "branch_22_rr"
storeID 2 0
cons 1
jumpA "block_24"

markA "branch_22_rr"
loadsubDI 0 2
bNEA "branch_22_dl"
cons 1
jumpA "block_24"

markA "branch_22_dl"
loadsubDI 0 3
bNEA "branch_22_dr"
cons 1
jumpA "block_24"

markA "branch_22_dr"
loadsubDI 0 4
bNEA "branch_22_ll"
storeID 7 0
div
jumpA "block_19"

markA "branch_22_ll"
loadsubDI 0 5
bNEA "branch_22_lr"
storeID 6 0
div
jumpA "block_19"

markA "branch_22_lr"
loadsubDI 0 6
bNEA "branch_22_ul"
div
jumpA "block_19"

markA "branch_22_ul"
loadsubDI 0 7
bNEA "branch_22_ur"
div
jumpA "block_19"

markA "branch_22_ur"
end

markA "block_23"
loadD 0
bNEA "branch_23_rl"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rl"
loadsubDI 0 1
bNEA "branch_23_rr"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rr"
loadsubDI 0 2
bNEA "branch_23_dl"
storeID 5 0
cons 1
jumpA "block_12"

markA "branch_23_dl"
loadsubDI 0 3
bNEA "branch_23_dr"
storeID 4 0
cons 1
jumpA "block_12"

markA "branch_23_dr"
loadsubDI 0 4
bNEA "branch_23_ll"
cons 1
jumpA "block_12"

markA "branch_23_ll"
loadsubDI 0 5
bNEA "branch_23_lr"
cons 1
jumpA "block_12"

markA "branch_23_lr"
loadsubDI 0 6
bNEA "branch_23_ul"
storeID 1 0
outputDMaybeS
jumpA "block_21"

markA "branch_23_ul"
loadsubDI 0 7
bNEA "branch_23_ur"
storeID 0 0
outputDMaybeS
jumpA "block_21"

markA "branch_23_ur"
end

markA "block_24"
loadD 0
bNEA "branch_24_rl"
lgt
jumpA "block_15"

markA "branch_24_rl"
loadsubDI 0 1
bNEA "branch_24_rr"
lgt
jumpA "block_15"

markA "branch_24_rr"
loadsubDI 0 2
bNEA "branch_24_dl"
storeID 1 0
lgt
jumpA "block_15"

markA "branch_24_dl"
loadsubDI 0 3
bNEA "branch_24_dr"
storeID 0 0
lgt
jumpA "block_15"

markA "branch_24_dr"
loadsubDI 0 4
bNEA "branch_24_ll"
storeID 0 0
lgt
jumpA "block_15"

markA "branch_24_ll"
loadsubDI 0 5
bNEA "branch_24_lr"
storeID 1 0
lgt
jumpA "block_15"

markA "branch_24_lr"
loadsubDI 0 6
bNEA "branch_24_ul"
storeID 1 0
lgt
jumpA "block_15"

markA "branch_24_ul"
loadsubDI 0 7
bNEA "branch_24_ur"
storeID 0 0
lgt
jumpA "block_15"

markA "branch_24_ur"
end
end
