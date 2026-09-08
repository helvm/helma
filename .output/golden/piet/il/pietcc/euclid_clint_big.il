storeid 0 0

markA "block_0"
loadd 0
bNEA "branch_0_rl"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_rl"
loadd 0
subI 1
bNEA "branch_0_rr"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_rr"
loadd 0
subI 2
bNEA "branch_0_dl"
inputDS
jumpA "block_3"

markA "branch_0_dl"
loadd 0
subI 3
bNEA "branch_0_dr"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_dr"
loadd 0
subI 4
bNEA "branch_0_ll"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_ll"
loadd 0
subI 5
bNEA "branch_0_lr"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_lr"
loadd 0
subI 6
bNEA "branch_0_ul"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_ul"
loadd 0
subI 7
bNEA "branch_0_ur"
storeid 2 0
inputDS
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_3"
loadd 0
bNEA "branch_3_rl"
storeid 3 0
inputDS
jumpA "block_5"

markA "branch_3_rl"
loadd 0
subI 1
bNEA "branch_3_rr"
storeid 2 0
inputDS
jumpA "block_5"

markA "branch_3_rr"
loadd 0
subI 2
bNEA "branch_3_dl"
inputDS
jumpA "block_5"

markA "branch_3_dl"
loadd 0
subI 3
bNEA "branch_3_dr"
inputDS
jumpA "block_5"

markA "branch_3_dr"
loadd 0
subI 4
bNEA "branch_3_ll"
storeid 7 0
mod
jumpA "block_0"

markA "branch_3_ll"
loadd 0
subI 5
bNEA "branch_3_lr"
storeid 6 0
mod
jumpA "block_0"

markA "branch_3_lr"
loadd 0
subI 6
bNEA "branch_3_ul"
mod
jumpA "block_0"

markA "branch_3_ul"
loadd 0
subI 7
bNEA "branch_3_ur"
mod
jumpA "block_0"

markA "branch_3_ur"
end

markA "block_4"
loadd 0
bNEA "branch_4_rl"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_rl"
loadd 0
subI 1
bNEA "branch_4_rr"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_rr"
loadd 0
subI 2
bNEA "branch_4_dl"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_dl"
loadd 0
subI 3
bNEA "branch_4_dr"
cons 4
jumpA "block_11"

markA "branch_4_dr"
loadd 0
subI 4
bNEA "branch_4_ll"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_ll"
loadd 0
subI 5
bNEA "branch_4_lr"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_lr"
loadd 0
subI 6
bNEA "branch_4_ul"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_ul"
loadd 0
subI 7
bNEA "branch_4_ur"
storeid 3 0
cons 4
jumpA "block_11"

markA "branch_4_ur"
end

markA "block_5"
loadd 0
bNEA "branch_5_rl"
storeid 3 0
copyI 0
jumpA "block_6"

markA "branch_5_rl"
loadd 0
subI 1
bNEA "branch_5_rr"
storeid 2 0
copyI 0
jumpA "block_6"

markA "branch_5_rr"
loadd 0
subI 2
bNEA "branch_5_dl"
copyI 0
jumpA "block_6"

markA "branch_5_dl"
loadd 0
subI 3
bNEA "branch_5_dr"
copyI 0
jumpA "block_6"

markA "branch_5_dr"
loadd 0
subI 4
bNEA "branch_5_ll"
storeid 7 0
mod
jumpA "block_3"

markA "branch_5_ll"
loadd 0
subI 5
bNEA "branch_5_lr"
storeid 6 0
mod
jumpA "block_3"

markA "branch_5_lr"
loadd 0
subI 6
bNEA "branch_5_ul"
mod
jumpA "block_3"

markA "branch_5_ul"
loadd 0
subI 7
bNEA "branch_5_ur"
mod
jumpA "block_3"

markA "branch_5_ur"
end

markA "block_6"
loadd 0
bNEA "branch_6_rl"
jumpA "block_8"

markA "branch_6_rl"
loadd 0
subI 1
bNEA "branch_6_rr"
jumpA "block_8"

markA "branch_6_rr"
loadd 0
subI 2
bNEA "branch_6_dl"
storeid 3 0
cons 3
jumpA "block_4"

markA "branch_6_dl"
loadd 0
subI 3
bNEA "branch_6_dr"
cons 3
jumpA "block_4"

markA "branch_6_dr"
loadd 0
subI 4
bNEA "branch_6_ll"
cons 3
jumpA "block_4"

markA "branch_6_ll"
loadd 0
subI 5
bNEA "branch_6_lr"
cons 3
jumpA "block_4"

markA "branch_6_lr"
loadd 0
subI 6
bNEA "branch_6_ul"
storeid 1 0
jumpA "block_8"

markA "branch_6_ul"
loadd 0
subI 7
bNEA "branch_6_ur"
storeid 0 0
jumpA "block_8"

markA "branch_6_ur"
end

markA "block_8"
loadd 0
bNEA "branch_8_rl"
div
jumpA "block_9"

markA "branch_8_rl"
loadd 0
subI 1
bNEA "branch_8_rr"
div
jumpA "block_9"

markA "branch_8_rr"
loadd 0
subI 2
bNEA "branch_8_dl"
storeid 5 0
jumpA "block_6"

markA "branch_8_dl"
loadd 0
subI 3
bNEA "branch_8_dr"
storeid 4 0
jumpA "block_6"

markA "branch_8_dr"
loadd 0
subI 4
bNEA "branch_8_ll"
jumpA "block_6"

markA "branch_8_ll"
loadd 0
subI 5
bNEA "branch_8_lr"
jumpA "block_6"

markA "branch_8_lr"
loadd 0
subI 6
bNEA "branch_8_ul"
storeid 1 0
div
jumpA "block_9"

markA "branch_8_ul"
loadd 0
subI 7
bNEA "branch_8_ur"
storeid 0 0
div
jumpA "block_9"

markA "branch_8_ur"
end

markA "block_9"
loadd 0
bNEA "branch_9_rl"
storeid 4 0
copyI 0
jumpA "block_8"

markA "branch_9_rl"
loadd 0
subI 1
bNEA "branch_9_rr"
storeid 5 0
copyI 0
jumpA "block_8"

markA "branch_9_rr"
loadd 0
subI 2
bNEA "branch_9_dl"
storeid 5 0
copyI 0
jumpA "block_8"

markA "branch_9_dl"
loadd 0
subI 3
bNEA "branch_9_dr"
storeid 4 0
copyI 0
jumpA "block_8"

markA "branch_9_dr"
loadd 0
subI 4
bNEA "branch_9_ll"
copyI 0
jumpA "block_8"

markA "branch_9_ll"
loadd 0
subI 5
bNEA "branch_9_lr"
copyI 0
jumpA "block_8"

markA "branch_9_lr"
loadd 0
subI 6
bNEA "branch_9_ul"
storeid 5 0
copyI 0
jumpA "block_8"

markA "branch_9_ul"
loadd 0
subI 7
bNEA "branch_9_ur"
storeid 4 0
copyI 0
jumpA "block_8"

markA "branch_9_ur"
end

markA "block_11"
loadd 0
bNEA "branch_11_rl"
storeid 3 0
roll
jumpA "block_14"

markA "branch_11_rl"
loadd 0
subI 1
bNEA "branch_11_rr"
storeid 2 0
roll
jumpA "block_14"

markA "branch_11_rr"
loadd 0
subI 2
bNEA "branch_11_dl"
roll
jumpA "block_14"

markA "branch_11_dl"
loadd 0
subI 3
bNEA "branch_11_dr"
roll
jumpA "block_14"

markA "branch_11_dr"
loadd 0
subI 4
bNEA "branch_11_ll"
storeid 7 0
discard
jumpA "block_4"

markA "branch_11_ll"
loadd 0
subI 5
bNEA "branch_11_lr"
storeid 6 0
discard
jumpA "block_4"

markA "branch_11_lr"
loadd 0
subI 6
bNEA "branch_11_ul"
discard
jumpA "block_4"

markA "branch_11_ul"
loadd 0
subI 7
bNEA "branch_11_ur"
discard
jumpA "block_4"

markA "branch_11_ur"
end

markA "block_12"
loadd 0
bNEA "branch_12_rl"
jumpA "block_9"

markA "branch_12_rl"
loadd 0
subI 1
bNEA "branch_12_rr"
discard
jumpA "block_23"

markA "branch_12_rr"
loadd 0
subI 2
bNEA "branch_12_dl"
storeid 1 0
discard
jumpA "block_23"

markA "branch_12_dl"
loadd 0
subI 3
bNEA "branch_12_dr"
storeid 0 0
jumpA "block_9"

markA "branch_12_dr"
loadd 0
subI 4
bNEA "branch_12_ll"
storeid 0 0
jumpA "block_9"

markA "branch_12_ll"
loadd 0
subI 5
bNEA "branch_12_lr"
storeid 1 0
discard
jumpA "block_23"

markA "branch_12_lr"
loadd 0
subI 6
bNEA "branch_12_ul"
storeid 1 0
discard
jumpA "block_23"

markA "branch_12_ul"
loadd 0
subI 7
bNEA "branch_12_ur"
storeid 0 0
jumpA "block_9"

markA "branch_12_ur"
end

markA "block_14"
loadd 0
bNEA "branch_14_rl"
storeid 2 0
mod
jumpA "block_19"

markA "branch_14_rl"
loadd 0
subI 1
bNEA "branch_14_rr"
storeid 2 0
mod
jumpA "block_19"

markA "branch_14_rr"
loadd 0
subI 2
bNEA "branch_14_dl"
mod
jumpA "block_19"

markA "branch_14_dl"
loadd 0
subI 3
bNEA "branch_14_dr"
storeid 2 0
mod
jumpA "block_19"

markA "branch_14_dr"
loadd 0
subI 4
bNEA "branch_14_ll"
storeid 6 0
lnot
jumpA "block_11"

markA "branch_14_ll"
loadd 0
subI 5
bNEA "branch_14_lr"
storeid 6 0
lnot
jumpA "block_11"

markA "branch_14_lr"
loadd 0
subI 6
bNEA "branch_14_ul"
lnot
jumpA "block_11"

markA "branch_14_ul"
loadd 0
subI 7
bNEA "branch_14_ur"
storeid 6 0
lnot
jumpA "block_11"

markA "branch_14_ur"
end

markA "block_15"
loadd 0
bNEA "branch_15_rl"
lnot
jumpA "block_16"

markA "branch_15_rl"
loadd 0
subI 1
bNEA "branch_15_rr"
storeid 0 0
lnot
jumpA "block_16"

markA "branch_15_rr"
loadd 0
subI 2
bNEA "branch_15_dl"
storeid 4 0
lgt
jumpA "block_24"

markA "branch_15_dl"
loadd 0
subI 3
bNEA "branch_15_dr"
storeid 4 0
lgt
jumpA "block_24"

markA "branch_15_dr"
loadd 0
subI 4
bNEA "branch_15_ll"
lgt
jumpA "block_24"

markA "branch_15_ll"
loadd 0
subI 5
bNEA "branch_15_lr"
storeid 4 0
lgt
jumpA "block_24"

markA "branch_15_lr"
loadd 0
subI 6
bNEA "branch_15_ul"
storeid 0 0
lnot
jumpA "block_16"

markA "branch_15_ul"
loadd 0
subI 7
bNEA "branch_15_ur"
storeid 0 0
lnot
jumpA "block_16"

markA "branch_15_ur"
end

markA "block_16"
loadd 0
bNEA "branch_16_rl"
modI 2
loadd 0
bxor
rstored 0
jumpA "block_12"

markA "branch_16_rl"
loadd 0
subI 1
bNEA "branch_16_rr"
modI 2
loadd 0
bxor
rstored 0
jumpA "block_12"

markA "branch_16_rr"
loadd 0
subI 2
bNEA "branch_16_dl"
storeid 5 0
roll
jumpA "block_15"

markA "branch_16_dl"
loadd 0
subI 3
bNEA "branch_16_dr"
storeid 4 0
roll
jumpA "block_15"

markA "branch_16_dr"
loadd 0
subI 4
bNEA "branch_16_ll"
roll
jumpA "block_15"

markA "branch_16_ll"
loadd 0
subI 5
bNEA "branch_16_lr"
roll
jumpA "block_15"

markA "branch_16_lr"
loadd 0
subI 6
bNEA "branch_16_ul"
storeid 1 0
modI 2
loadd 0
bxor
rstored 0
jumpA "block_12"

markA "branch_16_ul"
loadd 0
subI 7
bNEA "branch_16_ur"
storeid 0 0
modI 2
loadd 0
bxor
rstored 0
jumpA "block_12"

markA "branch_16_ur"
end

markA "block_19"
loadd 0
bNEA "branch_19_rl"
storeid 3 0
copyI 0
jumpA "block_22"

markA "branch_19_rl"
loadd 0
subI 1
bNEA "branch_19_rr"
storeid 2 0
copyI 0
jumpA "block_22"

markA "branch_19_rr"
loadd 0
subI 2
bNEA "branch_19_dl"
copyI 0
jumpA "block_22"

markA "branch_19_dl"
loadd 0
subI 3
bNEA "branch_19_dr"
copyI 0
jumpA "block_22"

markA "branch_19_dr"
loadd 0
subI 4
bNEA "branch_19_ll"
storeid 7 0
inputDS
jumpA "block_14"

markA "branch_19_ll"
loadd 0
subI 5
bNEA "branch_19_lr"
storeid 6 0
inputDS
jumpA "block_14"

markA "branch_19_lr"
loadd 0
subI 6
bNEA "branch_19_ul"
inputDS
jumpA "block_14"

markA "branch_19_ul"
loadd 0
subI 7
bNEA "branch_19_ur"
inputDS
jumpA "block_14"

markA "branch_19_ur"
end

markA "block_21"
end

markA "block_22"
loadd 0
bNEA "branch_22_rl"
storeid 3 0
cons 1
jumpA "block_24"

markA "branch_22_rl"
loadd 0
subI 1
bNEA "branch_22_rr"
storeid 2 0
cons 1
jumpA "block_24"

markA "branch_22_rr"
loadd 0
subI 2
bNEA "branch_22_dl"
cons 1
jumpA "block_24"

markA "branch_22_dl"
loadd 0
subI 3
bNEA "branch_22_dr"
cons 1
jumpA "block_24"

markA "branch_22_dr"
loadd 0
subI 4
bNEA "branch_22_ll"
storeid 7 0
div
jumpA "block_19"

markA "branch_22_ll"
loadd 0
subI 5
bNEA "branch_22_lr"
storeid 6 0
div
jumpA "block_19"

markA "branch_22_lr"
loadd 0
subI 6
bNEA "branch_22_ul"
div
jumpA "block_19"

markA "branch_22_ul"
loadd 0
subI 7
bNEA "branch_22_ur"
div
jumpA "block_19"

markA "branch_22_ur"
end

markA "block_23"
loadd 0
bNEA "branch_23_rl"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rl"
loadd 0
subI 1
bNEA "branch_23_rr"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rr"
loadd 0
subI 2
bNEA "branch_23_dl"
storeid 5 0
cons 1
jumpA "block_12"

markA "branch_23_dl"
loadd 0
subI 3
bNEA "branch_23_dr"
storeid 4 0
cons 1
jumpA "block_12"

markA "branch_23_dr"
loadd 0
subI 4
bNEA "branch_23_ll"
cons 1
jumpA "block_12"

markA "branch_23_ll"
loadd 0
subI 5
bNEA "branch_23_lr"
cons 1
jumpA "block_12"

markA "branch_23_lr"
loadd 0
subI 6
bNEA "branch_23_ul"
storeid 1 0
outputDMaybeS
jumpA "block_21"

markA "branch_23_ul"
loadd 0
subI 7
bNEA "branch_23_ur"
storeid 0 0
outputDMaybeS
jumpA "block_21"

markA "branch_23_ur"
end

markA "block_24"
loadd 0
bNEA "branch_24_rl"
lgt
jumpA "block_15"

markA "branch_24_rl"
loadd 0
subI 1
bNEA "branch_24_rr"
lgt
jumpA "block_15"

markA "branch_24_rr"
loadd 0
subI 2
bNEA "branch_24_dl"
storeid 1 0
lgt
jumpA "block_15"

markA "branch_24_dl"
loadd 0
subI 3
bNEA "branch_24_dr"
storeid 0 0
lgt
jumpA "block_15"

markA "branch_24_dr"
loadd 0
subI 4
bNEA "branch_24_ll"
storeid 0 0
lgt
jumpA "block_15"

markA "branch_24_ll"
loadd 0
subI 5
bNEA "branch_24_lr"
storeid 1 0
lgt
jumpA "block_15"

markA "branch_24_lr"
loadd 0
subI 6
bNEA "branch_24_ul"
storeid 1 0
lgt
jumpA "block_15"

markA "branch_24_ul"
loadd 0
subI 7
bNEA "branch_24_ur"
storeid 0 0
lgt
jumpA "block_15"

markA "branch_24_ur"
end
end
