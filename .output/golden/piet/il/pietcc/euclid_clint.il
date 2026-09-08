cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bNEA "branch_0_rl"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bNEA "branch_0_rr"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bNEA "branch_0_dl"
inputDS
jumpA "block_3"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bNEA "branch_0_dr"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_dr"
cons 0
load
cons 4
sub
bNEA "branch_0_ll"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_ll"
cons 0
load
cons 5
sub
bNEA "branch_0_lr"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_lr"
cons 0
load
cons 6
sub
bNEA "branch_0_ul"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_ul"
cons 0
load
cons 7
sub
bNEA "branch_0_ur"
cons 0
cons 2
store
inputDS
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_3"
cons 0
load
cons 0
sub
bNEA "branch_3_rl"
cons 0
cons 3
store
inputDS
jumpA "block_5"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bNEA "branch_3_rr"
cons 0
cons 2
store
inputDS
jumpA "block_5"

markA "branch_3_rr"
cons 0
load
cons 2
sub
bNEA "branch_3_dl"
inputDS
jumpA "block_5"

markA "branch_3_dl"
cons 0
load
cons 3
sub
bNEA "branch_3_dr"
inputDS
jumpA "block_5"

markA "branch_3_dr"
cons 0
load
cons 4
sub
bNEA "branch_3_ll"
cons 0
cons 7
store
mod
jumpA "block_0"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bNEA "branch_3_lr"
cons 0
cons 6
store
mod
jumpA "block_0"

markA "branch_3_lr"
cons 0
load
cons 6
sub
bNEA "branch_3_ul"
mod
jumpA "block_0"

markA "branch_3_ul"
cons 0
load
cons 7
sub
bNEA "branch_3_ur"
mod
jumpA "block_0"

markA "branch_3_ur"
end

markA "block_4"
cons 0
load
cons 0
sub
bNEA "branch_4_rl"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bNEA "branch_4_rr"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_rr"
cons 0
load
cons 2
sub
bNEA "branch_4_dl"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_dl"
cons 0
load
cons 3
sub
bNEA "branch_4_dr"
cons 4
jumpA "block_11"

markA "branch_4_dr"
cons 0
load
cons 4
sub
bNEA "branch_4_ll"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bNEA "branch_4_lr"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_lr"
cons 0
load
cons 6
sub
bNEA "branch_4_ul"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_ul"
cons 0
load
cons 7
sub
bNEA "branch_4_ur"
cons 0
cons 3
store
cons 4
jumpA "block_11"

markA "branch_4_ur"
end

markA "block_5"
cons 0
load
cons 0
sub
bNEA "branch_5_rl"
cons 0
cons 3
store
copyI 0
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bNEA "branch_5_rr"
cons 0
cons 2
store
copyI 0
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 2
sub
bNEA "branch_5_dl"
copyI 0
jumpA "block_6"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bNEA "branch_5_dr"
copyI 0
jumpA "block_6"

markA "branch_5_dr"
cons 0
load
cons 4
sub
bNEA "branch_5_ll"
cons 0
cons 7
store
mod
jumpA "block_3"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bNEA "branch_5_lr"
cons 0
cons 6
store
mod
jumpA "block_3"

markA "branch_5_lr"
cons 0
load
cons 6
sub
bNEA "branch_5_ul"
mod
jumpA "block_3"

markA "branch_5_ul"
cons 0
load
cons 7
sub
bNEA "branch_5_ur"
mod
jumpA "block_3"

markA "branch_5_ur"
end

markA "block_6"
cons 0
load
cons 0
sub
bNEA "branch_6_rl"
jumpA "block_8"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bNEA "branch_6_rr"
jumpA "block_8"

markA "branch_6_rr"
cons 0
load
cons 2
sub
bNEA "branch_6_dl"
cons 0
cons 3
store
cons 3
jumpA "block_4"

markA "branch_6_dl"
cons 0
load
cons 3
sub
bNEA "branch_6_dr"
cons 3
jumpA "block_4"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bNEA "branch_6_ll"
cons 3
jumpA "block_4"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bNEA "branch_6_lr"
cons 3
jumpA "block_4"

markA "branch_6_lr"
cons 0
load
cons 6
sub
bNEA "branch_6_ul"
cons 0
cons 1
store
jumpA "block_8"

markA "branch_6_ul"
cons 0
load
cons 7
sub
bNEA "branch_6_ur"
cons 0
cons 0
store
jumpA "block_8"

markA "branch_6_ur"
end

markA "block_8"
cons 0
load
cons 0
sub
bNEA "branch_8_rl"
div
jumpA "block_9"

markA "branch_8_rl"
cons 0
load
cons 1
sub
bNEA "branch_8_rr"
div
jumpA "block_9"

markA "branch_8_rr"
cons 0
load
cons 2
sub
bNEA "branch_8_dl"
cons 0
cons 5
store
jumpA "block_6"

markA "branch_8_dl"
cons 0
load
cons 3
sub
bNEA "branch_8_dr"
cons 0
cons 4
store
jumpA "block_6"

markA "branch_8_dr"
cons 0
load
cons 4
sub
bNEA "branch_8_ll"
jumpA "block_6"

markA "branch_8_ll"
cons 0
load
cons 5
sub
bNEA "branch_8_lr"
jumpA "block_6"

markA "branch_8_lr"
cons 0
load
cons 6
sub
bNEA "branch_8_ul"
cons 0
cons 1
store
div
jumpA "block_9"

markA "branch_8_ul"
cons 0
load
cons 7
sub
bNEA "branch_8_ur"
cons 0
cons 0
store
div
jumpA "block_9"

markA "branch_8_ur"
end

markA "block_9"
cons 0
load
cons 0
sub
bNEA "branch_9_rl"
cons 0
cons 4
store
copyI 0
jumpA "block_8"

markA "branch_9_rl"
cons 0
load
cons 1
sub
bNEA "branch_9_rr"
cons 0
cons 5
store
copyI 0
jumpA "block_8"

markA "branch_9_rr"
cons 0
load
cons 2
sub
bNEA "branch_9_dl"
cons 0
cons 5
store
copyI 0
jumpA "block_8"

markA "branch_9_dl"
cons 0
load
cons 3
sub
bNEA "branch_9_dr"
cons 0
cons 4
store
copyI 0
jumpA "block_8"

markA "branch_9_dr"
cons 0
load
cons 4
sub
bNEA "branch_9_ll"
copyI 0
jumpA "block_8"

markA "branch_9_ll"
cons 0
load
cons 5
sub
bNEA "branch_9_lr"
copyI 0
jumpA "block_8"

markA "branch_9_lr"
cons 0
load
cons 6
sub
bNEA "branch_9_ul"
cons 0
cons 5
store
copyI 0
jumpA "block_8"

markA "branch_9_ul"
cons 0
load
cons 7
sub
bNEA "branch_9_ur"
cons 0
cons 4
store
copyI 0
jumpA "block_8"

markA "branch_9_ur"
end

markA "block_11"
cons 0
load
cons 0
sub
bNEA "branch_11_rl"
cons 0
cons 3
store
roll
jumpA "block_14"

markA "branch_11_rl"
cons 0
load
cons 1
sub
bNEA "branch_11_rr"
cons 0
cons 2
store
roll
jumpA "block_14"

markA "branch_11_rr"
cons 0
load
cons 2
sub
bNEA "branch_11_dl"
roll
jumpA "block_14"

markA "branch_11_dl"
cons 0
load
cons 3
sub
bNEA "branch_11_dr"
roll
jumpA "block_14"

markA "branch_11_dr"
cons 0
load
cons 4
sub
bNEA "branch_11_ll"
cons 0
cons 7
store
discard
jumpA "block_4"

markA "branch_11_ll"
cons 0
load
cons 5
sub
bNEA "branch_11_lr"
cons 0
cons 6
store
discard
jumpA "block_4"

markA "branch_11_lr"
cons 0
load
cons 6
sub
bNEA "branch_11_ul"
discard
jumpA "block_4"

markA "branch_11_ul"
cons 0
load
cons 7
sub
bNEA "branch_11_ur"
discard
jumpA "block_4"

markA "branch_11_ur"
end

markA "block_12"
cons 0
load
cons 0
sub
bNEA "branch_12_rl"
jumpA "block_9"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bNEA "branch_12_rr"
discard
jumpA "block_23"

markA "branch_12_rr"
cons 0
load
cons 2
sub
bNEA "branch_12_dl"
cons 0
cons 1
store
discard
jumpA "block_23"

markA "branch_12_dl"
cons 0
load
cons 3
sub
bNEA "branch_12_dr"
cons 0
cons 0
store
jumpA "block_9"

markA "branch_12_dr"
cons 0
load
cons 4
sub
bNEA "branch_12_ll"
cons 0
cons 0
store
jumpA "block_9"

markA "branch_12_ll"
cons 0
load
cons 5
sub
bNEA "branch_12_lr"
cons 0
cons 1
store
discard
jumpA "block_23"

markA "branch_12_lr"
cons 0
load
cons 6
sub
bNEA "branch_12_ul"
cons 0
cons 1
store
discard
jumpA "block_23"

markA "branch_12_ul"
cons 0
load
cons 7
sub
bNEA "branch_12_ur"
cons 0
cons 0
store
jumpA "block_9"

markA "branch_12_ur"
end

markA "block_14"
cons 0
load
cons 0
sub
bNEA "branch_14_rl"
cons 0
cons 2
store
mod
jumpA "block_19"

markA "branch_14_rl"
cons 0
load
cons 1
sub
bNEA "branch_14_rr"
cons 0
cons 2
store
mod
jumpA "block_19"

markA "branch_14_rr"
cons 0
load
cons 2
sub
bNEA "branch_14_dl"
mod
jumpA "block_19"

markA "branch_14_dl"
cons 0
load
cons 3
sub
bNEA "branch_14_dr"
cons 0
cons 2
store
mod
jumpA "block_19"

markA "branch_14_dr"
cons 0
load
cons 4
sub
bNEA "branch_14_ll"
cons 0
cons 6
store
lnot
jumpA "block_11"

markA "branch_14_ll"
cons 0
load
cons 5
sub
bNEA "branch_14_lr"
cons 0
cons 6
store
lnot
jumpA "block_11"

markA "branch_14_lr"
cons 0
load
cons 6
sub
bNEA "branch_14_ul"
lnot
jumpA "block_11"

markA "branch_14_ul"
cons 0
load
cons 7
sub
bNEA "branch_14_ur"
cons 0
cons 6
store
lnot
jumpA "block_11"

markA "branch_14_ur"
end

markA "block_15"
cons 0
load
cons 0
sub
bNEA "branch_15_rl"
lnot
jumpA "block_16"

markA "branch_15_rl"
cons 0
load
cons 1
sub
bNEA "branch_15_rr"
cons 0
cons 0
store
lnot
jumpA "block_16"

markA "branch_15_rr"
cons 0
load
cons 2
sub
bNEA "branch_15_dl"
cons 0
cons 4
store
lgt
jumpA "block_24"

markA "branch_15_dl"
cons 0
load
cons 3
sub
bNEA "branch_15_dr"
cons 0
cons 4
store
lgt
jumpA "block_24"

markA "branch_15_dr"
cons 0
load
cons 4
sub
bNEA "branch_15_ll"
lgt
jumpA "block_24"

markA "branch_15_ll"
cons 0
load
cons 5
sub
bNEA "branch_15_lr"
cons 0
cons 4
store
lgt
jumpA "block_24"

markA "branch_15_lr"
cons 0
load
cons 6
sub
bNEA "branch_15_ul"
cons 0
cons 0
store
lnot
jumpA "block_16"

markA "branch_15_ul"
cons 0
load
cons 7
sub
bNEA "branch_15_ur"
cons 0
cons 0
store
lnot
jumpA "block_16"

markA "branch_15_ur"
end

markA "block_16"
cons 0
load
cons 0
sub
bNEA "branch_16_rl"
cons 2
mod
cons 0
load
bxor
cons 0
rstore
jumpA "block_12"

markA "branch_16_rl"
cons 0
load
cons 1
sub
bNEA "branch_16_rr"
cons 2
mod
cons 0
load
bxor
cons 0
rstore
jumpA "block_12"

markA "branch_16_rr"
cons 0
load
cons 2
sub
bNEA "branch_16_dl"
cons 0
cons 5
store
roll
jumpA "block_15"

markA "branch_16_dl"
cons 0
load
cons 3
sub
bNEA "branch_16_dr"
cons 0
cons 4
store
roll
jumpA "block_15"

markA "branch_16_dr"
cons 0
load
cons 4
sub
bNEA "branch_16_ll"
roll
jumpA "block_15"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bNEA "branch_16_lr"
roll
jumpA "block_15"

markA "branch_16_lr"
cons 0
load
cons 6
sub
bNEA "branch_16_ul"
cons 0
cons 1
store
cons 2
mod
cons 0
load
bxor
cons 0
rstore
jumpA "block_12"

markA "branch_16_ul"
cons 0
load
cons 7
sub
bNEA "branch_16_ur"
cons 0
cons 0
store
cons 2
mod
cons 0
load
bxor
cons 0
rstore
jumpA "block_12"

markA "branch_16_ur"
end

markA "block_19"
cons 0
load
cons 0
sub
bNEA "branch_19_rl"
cons 0
cons 3
store
copyI 0
jumpA "block_22"

markA "branch_19_rl"
cons 0
load
cons 1
sub
bNEA "branch_19_rr"
cons 0
cons 2
store
copyI 0
jumpA "block_22"

markA "branch_19_rr"
cons 0
load
cons 2
sub
bNEA "branch_19_dl"
copyI 0
jumpA "block_22"

markA "branch_19_dl"
cons 0
load
cons 3
sub
bNEA "branch_19_dr"
copyI 0
jumpA "block_22"

markA "branch_19_dr"
cons 0
load
cons 4
sub
bNEA "branch_19_ll"
cons 0
cons 7
store
inputDS
jumpA "block_14"

markA "branch_19_ll"
cons 0
load
cons 5
sub
bNEA "branch_19_lr"
cons 0
cons 6
store
inputDS
jumpA "block_14"

markA "branch_19_lr"
cons 0
load
cons 6
sub
bNEA "branch_19_ul"
inputDS
jumpA "block_14"

markA "branch_19_ul"
cons 0
load
cons 7
sub
bNEA "branch_19_ur"
inputDS
jumpA "block_14"

markA "branch_19_ur"
end

markA "block_21"
end

markA "block_22"
cons 0
load
cons 0
sub
bNEA "branch_22_rl"
cons 0
cons 3
store
cons 1
jumpA "block_24"

markA "branch_22_rl"
cons 0
load
cons 1
sub
bNEA "branch_22_rr"
cons 0
cons 2
store
cons 1
jumpA "block_24"

markA "branch_22_rr"
cons 0
load
cons 2
sub
bNEA "branch_22_dl"
cons 1
jumpA "block_24"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bNEA "branch_22_dr"
cons 1
jumpA "block_24"

markA "branch_22_dr"
cons 0
load
cons 4
sub
bNEA "branch_22_ll"
cons 0
cons 7
store
div
jumpA "block_19"

markA "branch_22_ll"
cons 0
load
cons 5
sub
bNEA "branch_22_lr"
cons 0
cons 6
store
div
jumpA "block_19"

markA "branch_22_lr"
cons 0
load
cons 6
sub
bNEA "branch_22_ul"
div
jumpA "block_19"

markA "branch_22_ul"
cons 0
load
cons 7
sub
bNEA "branch_22_ur"
div
jumpA "block_19"

markA "branch_22_ur"
end

markA "block_23"
cons 0
load
cons 0
sub
bNEA "branch_23_rl"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bNEA "branch_23_rr"
outputDMaybeS
jumpA "block_21"

markA "branch_23_rr"
cons 0
load
cons 2
sub
bNEA "branch_23_dl"
cons 0
cons 5
store
cons 1
jumpA "block_12"

markA "branch_23_dl"
cons 0
load
cons 3
sub
bNEA "branch_23_dr"
cons 0
cons 4
store
cons 1
jumpA "block_12"

markA "branch_23_dr"
cons 0
load
cons 4
sub
bNEA "branch_23_ll"
cons 1
jumpA "block_12"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bNEA "branch_23_lr"
cons 1
jumpA "block_12"

markA "branch_23_lr"
cons 0
load
cons 6
sub
bNEA "branch_23_ul"
cons 0
cons 1
store
outputDMaybeS
jumpA "block_21"

markA "branch_23_ul"
cons 0
load
cons 7
sub
bNEA "branch_23_ur"
cons 0
cons 0
store
outputDMaybeS
jumpA "block_21"

markA "branch_23_ur"
end

markA "block_24"
cons 0
load
cons 0
sub
bNEA "branch_24_rl"
lgt
jumpA "block_15"

markA "branch_24_rl"
cons 0
load
cons 1
sub
bNEA "branch_24_rr"
lgt
jumpA "block_15"

markA "branch_24_rr"
cons 0
load
cons 2
sub
bNEA "branch_24_dl"
cons 0
cons 1
store
lgt
jumpA "block_15"

markA "branch_24_dl"
cons 0
load
cons 3
sub
bNEA "branch_24_dr"
cons 0
cons 0
store
lgt
jumpA "block_15"

markA "branch_24_dr"
cons 0
load
cons 4
sub
bNEA "branch_24_ll"
cons 0
cons 0
store
lgt
jumpA "block_15"

markA "branch_24_ll"
cons 0
load
cons 5
sub
bNEA "branch_24_lr"
cons 0
cons 1
store
lgt
jumpA "block_15"

markA "branch_24_lr"
cons 0
load
cons 6
sub
bNEA "branch_24_ul"
cons 0
cons 1
store
lgt
jumpA "block_15"

markA "branch_24_ul"
cons 0
load
cons 7
sub
bNEA "branch_24_ur"
cons 0
cons 0
store
lgt
jumpA "block_15"

markA "branch_24_ur"
end
end
