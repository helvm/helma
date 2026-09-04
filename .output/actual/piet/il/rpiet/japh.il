cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 1
jumpA "block_1"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 1
jumpA "block_1"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_9"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bEZA "branch_0_dr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_9"

markA "branch_0_dr"

markA "block_1"
cons 0
load
cons 0
sub
bEZA "branch_1_rl"
cons 1
jumpA "block_2"

markA "branch_1_rl"
cons 0
load
cons 1
sub
bEZA "branch_1_rr"
cons 1
jumpA "block_2"

markA "branch_1_rr"
cons 0
load
cons 2
sub
bEZA "branch_1_dl"
discard
jumpA "block_10"

markA "branch_1_dl"
cons 0
load
cons 3
sub
bEZA "branch_1_dr"
discard
jumpA "block_10"

markA "branch_1_dr"
cons 0
load
cons 4
sub
bEZA "branch_1_ll"
discard
jumpA "block_0"

markA "branch_1_ll"
cons 0
load
cons 5
sub
bEZA "branch_1_lr"
discard
jumpA "block_0"

markA "branch_1_lr"

markA "block_2"
cons 0
load
cons 0
sub
bEZA "branch_2_rl"
jumpA "block_4"

markA "branch_2_rl"
cons 0
load
cons 1
sub
bEZA "branch_2_rr"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_11"

markA "branch_2_rr"
cons 0
load
cons 2
sub
bEZA "branch_2_dl"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_2_dl"
cons 0
load
cons 3
sub
bEZA "branch_2_dr"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_2_dr"
cons 0
load
cons 4
sub
bEZA "branch_2_ll"
cons 2
jumpA "block_10"

markA "branch_2_ll"
cons 0
load
cons 5
sub
bEZA "branch_2_lr"
discard
jumpA "block_1"

markA "branch_2_lr"

markA "block_4"
cons 0
load
cons 0
sub
bEZA "branch_4_rl"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_5"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bEZA "branch_4_rr"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_5"

markA "branch_4_rr"
cons 0
load
cons 2
sub
bEZA "branch_4_dl"
jumpA "block_16"

markA "branch_4_dl"
cons 0
load
cons 3
sub
bEZA "branch_4_dr"
jumpA "block_16"

markA "branch_4_dr"
cons 0
load
cons 4
sub
bEZA "branch_4_ll"
jumpA "block_2"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bEZA "branch_4_lr"
jumpA "block_2"

markA "branch_4_lr"

markA "block_5"
cons 0
load
cons 0
sub
bEZA "branch_5_rl"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
cons 0
cons 2
store
jumpA "block_18"

markA "branch_5_rr"
cons 0
load
cons 2
sub
bEZA "branch_5_dl"
jumpA "block_14"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bEZA "branch_5_dr"
jumpA "block_14"

markA "branch_5_dr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_4"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bEZA "branch_5_lr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_4"

markA "branch_5_lr"

markA "block_9"
cons 0
load
cons 0
sub
bEZA "branch_9_rl"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_10"

markA "branch_9_rl"
cons 0
load
cons 1
sub
bEZA "branch_9_rr"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_10"

markA "branch_9_rr"
cons 0
load
cons 2
sub
bEZA "branch_9_dl"
cons 0
cons 6
store
jumpA "block_9"

markA "branch_9_dl"
cons 0
load
cons 3
sub
bEZA "branch_9_dr"
cons 0
cons 7
store
jumpA "block_9"

markA "branch_9_dr"
cons 0
load
cons 6
sub
bEZA "branch_9_ul"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_0"

markA "branch_9_ul"
cons 0
load
cons 7
sub
bEZA "branch_9_ur"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_0"

markA "branch_9_ur"

markA "block_10"
cons 0
load
cons 0
sub
bEZA "branch_10_rl"
discard
jumpA "block_2"

markA "branch_10_rl"
cons 0
load
cons 1
sub
bEZA "branch_10_rr"
discard
jumpA "block_2"

markA "branch_10_rr"
cons 0
load
cons 2
sub
bEZA "branch_10_dl"
jumpA "block_22"

markA "branch_10_dl"
cons 0
load
cons 3
sub
bEZA "branch_10_dr"
jumpA "block_22"

markA "branch_10_dr"
cons 0
load
cons 4
sub
bEZA "branch_10_ll"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_9"

markA "branch_10_ll"
cons 0
load
cons 5
sub
bEZA "branch_10_lr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_9"

markA "branch_10_lr"
cons 0
load
cons 6
sub
bEZA "branch_10_ul"
cons 1
jumpA "block_1"

markA "branch_10_ul"
cons 0
load
cons 7
sub
bEZA "branch_10_ur"
cons 1
jumpA "block_1"

markA "branch_10_ur"

markA "block_11"
cons 0
load
cons 0
sub
bEZA "branch_11_rl"
jumpA "block_12"

markA "branch_11_rl"
cons 0
load
cons 1
sub
bEZA "branch_11_rr"
jumpA "block_12"

markA "branch_11_rr"
cons 0
load
cons 2
sub
bEZA "branch_11_dl"
jumpA "block_23"

markA "branch_11_dl"
cons 0
load
cons 3
sub
bEZA "branch_11_dr"
jumpA "block_23"

markA "branch_11_dr"
cons 0
load
cons 4
sub
bEZA "branch_11_ll"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_2"

markA "branch_11_ll"
cons 0
load
cons 5
sub
bEZA "branch_11_lr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_2"

markA "branch_11_lr"
cons 0
load
cons 6
sub
bEZA "branch_11_ul"
cons 0
cons 1
store
jumpA "block_4"

markA "branch_11_ul"
cons 0
load
cons 7
sub
bEZA "branch_11_ur"
cons 0
cons 0
store
jumpA "block_4"

markA "branch_11_ur"

markA "block_12"
cons 0
load
cons 0
sub
bEZA "branch_12_rl"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bEZA "branch_12_rr"
cons 0
cons 2
store
jumpA "block_18"

markA "branch_12_rr"
cons 0
load
cons 2
sub
bEZA "branch_12_dl"
jumpA "block_16"

markA "branch_12_dl"
cons 0
load
cons 3
sub
bEZA "branch_12_dr"
jumpA "block_16"

markA "branch_12_dr"
cons 0
load
cons 4
sub
bEZA "branch_12_ll"
jumpA "block_11"

markA "branch_12_ll"
cons 0
load
cons 5
sub
bEZA "branch_12_lr"
jumpA "block_11"

markA "branch_12_lr"
cons 0
load
cons 6
sub
bEZA "branch_12_ul"
cons 0
cons 2
store
jumpA "block_18"

markA "branch_12_ul"
cons 0
load
cons 7
sub
bEZA "branch_12_ur"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_12_ur"

markA "block_14"
cons 0
load
cons 0
sub
bEZA "branch_14_rl"
jumpA "block_15"

markA "branch_14_rl"
cons 0
load
cons 1
sub
bEZA "branch_14_rr"
jumpA "block_15"

markA "branch_14_rr"
cons 0
load
cons 2
sub
bEZA "branch_14_dl"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_14_dl"
cons 0
load
cons 3
sub
bEZA "branch_14_dr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_14_dr"
cons 0
load
cons 4
sub
bEZA "branch_14_ll"
cons 0
cons 7
store
jumpA "block_9"

markA "branch_14_ll"
cons 0
load
cons 5
sub
bEZA "branch_14_lr"
cons 0
cons 6
store
jumpA "block_9"

markA "branch_14_lr"
cons 0
load
cons 6
sub
bEZA "branch_14_ul"
jumpA "block_5"

markA "branch_14_ul"
cons 0
load
cons 7
sub
bEZA "branch_14_ur"
jumpA "block_5"

markA "branch_14_ur"

markA "block_15"
cons 0
load
cons 0
sub
bEZA "branch_15_rl"
cons 0
cons 3
store
jumpA "block_26"

markA "branch_15_rl"
cons 0
load
cons 1
sub
bEZA "branch_15_rr"
cons 0
cons 2
store
jumpA "block_26"

markA "branch_15_rr"
cons 0
load
cons 2
sub
bEZA "branch_15_dl"
discard
jumpA "block_16"

markA "branch_15_dl"
cons 0
load
cons 3
sub
bEZA "branch_15_dr"
discard
jumpA "block_16"

markA "branch_15_dr"
cons 0
load
cons 4
sub
bEZA "branch_15_ll"
jumpA "block_14"

markA "branch_15_ll"
cons 0
load
cons 5
sub
bEZA "branch_15_lr"
jumpA "block_14"

markA "branch_15_lr"
cons 0
load
cons 6
sub
bEZA "branch_15_ul"
cons 0
cons 2
store
jumpA "block_18"

markA "branch_15_ul"
cons 0
load
cons 7
sub
bEZA "branch_15_ur"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_15_ur"

markA "block_16"
cons 0
load
cons 0
sub
bEZA "branch_16_rl"
jumpA "block_17"

markA "branch_16_rl"
cons 0
load
cons 1
sub
bEZA "branch_16_rr"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_16_rr"
cons 0
load
cons 2
sub
bEZA "branch_16_dl"
cons 38
jumpA "block_29"

markA "branch_16_dl"
cons 0
load
cons 3
sub
bEZA "branch_16_dr"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_16_dr"
cons 0
load
cons 4
sub
bEZA "branch_16_ll"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bEZA "branch_16_lr"
cons 0
cons 6
store
jumpA "block_9"

markA "branch_16_lr"
cons 0
load
cons 6
sub
bEZA "branch_16_ul"
jumpA "block_4"

markA "branch_16_ul"
cons 0
load
cons 7
sub
bEZA "branch_16_ur"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_16_ur"

markA "block_17"
cons 0
load
cons 0
sub
bEZA "branch_17_rl"
cons 0
cons 3
store
jumpA "block_26"

markA "branch_17_rl"
cons 0
load
cons 1
sub
bEZA "branch_17_rr"
cons 0
cons 2
store
jumpA "block_26"

markA "branch_17_rr"
cons 0
load
cons 2
sub
bEZA "branch_17_dl"
sub
jumpA "block_25"

markA "branch_17_dl"
cons 0
load
cons 3
sub
bEZA "branch_17_dr"
sub
jumpA "block_25"

markA "branch_17_dr"
cons 0
load
cons 4
sub
bEZA "branch_17_ll"
jumpA "block_20"

markA "branch_17_ll"
cons 0
load
cons 5
sub
bEZA "branch_17_lr"
jumpA "block_16"

markA "branch_17_lr"
cons 0
load
cons 6
sub
bEZA "branch_17_ul"
cons 0
cons 2
store
jumpA "block_26"

markA "branch_17_ul"
cons 0
load
cons 7
sub
bEZA "branch_17_ur"
cons 0
cons 3
store
jumpA "block_26"

markA "branch_17_ur"

markA "block_18"
cons 0
load
cons 0
sub
bEZA "branch_18_rl"
jumpA "block_17"

markA "branch_18_rl"
cons 0
load
cons 1
sub
bEZA "branch_18_rr"
jumpA "block_17"

markA "branch_18_rr"
cons 0
load
cons 2
sub
bEZA "branch_18_dl"
cons 1
jumpA "block_20"

markA "branch_18_dl"
cons 0
load
cons 3
sub
bEZA "branch_18_dr"
cons 1
jumpA "block_20"

markA "branch_18_dr"
cons 0
load
cons 4
sub
bEZA "branch_18_ll"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_18_ll"
cons 0
load
cons 5
sub
bEZA "branch_18_lr"
cons 0
load
cons 2
mod
add
cons 2
mod
cons 0
load
cons 2
div
cons 2
mul
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_18_lr"
cons 0
load
cons 6
sub
bEZA "branch_18_ul"
cons 0
cons 2
store
jumpA "block_18"

markA "branch_18_ul"
cons 0
load
cons 7
sub
bEZA "branch_18_ur"
cons 0
cons 3
store
jumpA "block_18"

markA "branch_18_ur"

markA "block_20"
cons 0
load
cons 0
sub
bEZA "branch_20_rl"
jumpA "block_17"

markA "branch_20_rl"
cons 0
load
cons 1
sub
bEZA "branch_20_rr"
discard
jumpA "block_24"

markA "branch_20_rr"
cons 0
load
cons 2
sub
bEZA "branch_20_dl"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_20_dl"
cons 0
load
cons 3
sub
bEZA "branch_20_dr"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_20_dr"
cons 0
load
cons 4
sub
bEZA "branch_20_ll"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_20_ll"
cons 0
load
cons 5
sub
bEZA "branch_20_lr"
cons 0
load
cons 2
div
add
cons 4
mod
cons 2
mul
cons 0
load
cons 2
mod
add
cons 0
halibut
store
jumpA "block_16"

markA "branch_20_lr"
cons 0
load
cons 6
sub
bEZA "branch_20_ul"
discard
jumpA "block_18"

markA "branch_20_ul"
cons 0
load
cons 7
sub
bEZA "branch_20_ur"
discard
jumpA "block_18"

markA "branch_20_ur"

markA "block_21"
cons 0
load
cons 0
sub
bEZA "branch_21_rl"
mio outputchar
jumpA "block_22"

markA "branch_21_rl"
cons 0
load
cons 1
sub
bEZA "branch_21_rr"
mio outputchar
jumpA "block_22"

markA "branch_21_rr"
cons 0
load
cons 2
sub
bEZA "branch_21_dl"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_21_dl"
cons 0
load
cons 3
sub
bEZA "branch_21_dr"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_21_dr"

markA "block_22"
cons 0
load
cons 0
sub
bEZA "branch_22_rl"
jumpA "block_23"

markA "branch_22_rl"
cons 0
load
cons 1
sub
bEZA "branch_22_rr"
jumpA "block_16"

markA "branch_22_rr"
cons 0
load
cons 2
sub
bEZA "branch_22_dl"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bEZA "branch_22_dr"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_22_dr"
cons 0
load
cons 4
sub
bEZA "branch_22_ll"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_22_ll"
cons 0
load
cons 5
sub
bEZA "branch_22_lr"
sub
jumpA "block_21"

markA "branch_22_lr"
cons 0
load
cons 6
sub
bEZA "branch_22_ul"
jumpA "block_10"

markA "branch_22_ul"
cons 0
load
cons 7
sub
bEZA "branch_22_ur"
jumpA "block_10"

markA "branch_22_ur"

markA "block_23"
cons 0
load
cons 0
sub
bEZA "branch_23_rl"
discard
jumpA "block_16"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bEZA "branch_23_rr"
discard
jumpA "block_16"

markA "branch_23_rr"
cons 0
load
cons 2
sub
bEZA "branch_23_dl"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_23_dl"
cons 0
load
cons 3
sub
bEZA "branch_23_dr"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_23_dr"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
jumpA "block_22"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
jumpA "block_22"

markA "branch_23_lr"
cons 0
load
cons 6
sub
bEZA "branch_23_ul"
jumpA "block_11"

markA "branch_23_ul"
cons 0
load
cons 7
sub
bEZA "branch_23_ur"
jumpA "block_11"

markA "branch_23_ur"

markA "block_24"
cons 0
load
cons 0
sub
bEZA "branch_24_rl"
sub
jumpA "block_25"

markA "branch_24_rl"
cons 0
load
cons 1
sub
bEZA "branch_24_rr"
sub
jumpA "block_25"

markA "branch_24_rr"
cons 0
load
cons 2
sub
bEZA "branch_24_dl"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_24_dl"
cons 0
load
cons 3
sub
bEZA "branch_24_dr"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_24_dr"
cons 0
load
cons 4
sub
bEZA "branch_24_ll"
cons 1
jumpA "block_20"

markA "branch_24_ll"
cons 0
load
cons 5
sub
bEZA "branch_24_lr"
cons 1
jumpA "block_20"

markA "branch_24_lr"
cons 0
load
cons 6
sub
bEZA "branch_24_ul"
cons 0
cons 2
store
jumpA "block_24"

markA "branch_24_ul"
cons 0
load
cons 7
sub
bEZA "branch_24_ur"
cons 0
cons 3
store
jumpA "block_24"

markA "branch_24_ur"

markA "block_25"
cons 0
load
cons 0
sub
bEZA "branch_25_rl"
cons 2
jumpA "block_26"

markA "branch_25_rl"
cons 0
load
cons 1
sub
bEZA "branch_25_rr"
cons 2
jumpA "block_26"

markA "branch_25_rr"
cons 0
load
cons 2
sub
bEZA "branch_25_dl"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_25_dl"
cons 0
load
cons 3
sub
bEZA "branch_25_dr"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_25_dr"
cons 0
load
cons 4
sub
bEZA "branch_25_ll"
mio outputchar
jumpA "block_24"

markA "branch_25_ll"
cons 0
load
cons 5
sub
bEZA "branch_25_lr"
mio outputchar
jumpA "block_24"

markA "branch_25_lr"
cons 0
load
cons 6
sub
bEZA "branch_25_ul"
mio outputchar
jumpA "block_17"

markA "branch_25_ul"
cons 0
load
cons 7
sub
bEZA "branch_25_ur"
cons 0
cons 3
store
jumpA "block_26"

markA "branch_25_ur"

markA "block_26"
cons 0
load
cons 4
sub
bEZA "branch_26_ll"
discard
jumpA "block_25"

markA "branch_26_ll"
cons 0
load
cons 5
sub
bEZA "branch_26_lr"
discard
jumpA "block_25"

markA "branch_26_lr"
cons 0
load
cons 6
sub
bEZA "branch_26_ul"
cons 0
cons 2
store
jumpA "block_26"

markA "branch_26_ul"
cons 0
load
cons 7
sub
bEZA "branch_26_ur"
cons 0
cons 3
store
jumpA "block_26"

markA "branch_26_ur"

markA "block_29"
cons 0
load
cons 0
sub
bEZA "branch_29_rl"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_29_rl"
cons 0
load
cons 1
sub
bEZA "branch_29_rr"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_29_rr"
cons 0
load
cons 2
sub
bEZA "branch_29_dl"
jumpA "block_31"

markA "branch_29_dl"
cons 0
load
cons 3
sub
bEZA "branch_29_dr"
jumpA "block_31"

markA "branch_29_dr"
cons 0
load
cons 4
sub
bEZA "branch_29_ll"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_29_ll"
cons 0
load
cons 5
sub
bEZA "branch_29_lr"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_29_lr"
cons 0
load
cons 6
sub
bEZA "branch_29_ul"
discard
jumpA "block_16"

markA "branch_29_ul"
cons 0
load
cons 7
sub
bEZA "branch_29_ur"
discard
jumpA "block_16"

markA "branch_29_ur"

markA "block_31"
cons 0
load
cons 0
sub
bEZA "branch_31_rl"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_31_rl"
cons 0
load
cons 1
sub
bEZA "branch_31_rr"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_31_rr"
cons 0
load
cons 2
sub
bEZA "branch_31_dl"
sub
jumpA "block_33"

markA "branch_31_dl"
cons 0
load
cons 3
sub
bEZA "branch_31_dr"
sub
jumpA "block_33"

markA "branch_31_dr"
cons 0
load
cons 4
sub
bEZA "branch_31_ll"
cons 0
cons 7
store
jumpA "block_21"

markA "branch_31_ll"
cons 0
load
cons 5
sub
bEZA "branch_31_lr"
cons 0
cons 6
store
jumpA "block_21"

markA "branch_31_lr"
cons 0
load
cons 6
sub
bEZA "branch_31_ul"
jumpA "block_29"

markA "branch_31_ul"
cons 0
load
cons 7
sub
bEZA "branch_31_ur"
jumpA "block_29"

markA "branch_31_ur"

markA "block_33"
cons 0
load
cons 0
sub
bEZA "branch_33_rl"
cons 0
cons 4
store
jumpA "block_33"

markA "branch_33_rl"
cons 0
load
cons 1
sub
bEZA "branch_33_rr"
cons 0
cons 5
store
jumpA "block_33"

markA "branch_33_rr"
cons 0
load
cons 6
sub
bEZA "branch_33_ul"
mio outputchar
jumpA "block_31"

markA "branch_33_ul"
cons 0
load
cons 7
sub
bEZA "branch_33_ur"
mio outputchar
jumpA "block_31"

markA "branch_33_ur"
end
