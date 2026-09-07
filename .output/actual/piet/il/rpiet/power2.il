cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
mio inputdec
jumpA "block_3"

markA "branch_0_rl"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
mio inputchar
jumpA "block_37"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bEZA "branch_0_dr"
mio inputchar
jumpA "block_37"

markA "branch_0_dr"

markA "block_3"
cons 0
load
cons 0
sub
bEZA "branch_3_rl"
mio inputdec
jumpA "block_4"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bEZA "branch_3_rr"
mio inputdec
jumpA "block_4"

markA "branch_3_rr"
cons 0
load
cons 4
sub
bEZA "branch_3_ll"
mod
jumpA "block_0"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bEZA "branch_3_lr"
mod
jumpA "block_0"

markA "branch_3_lr"

markA "block_4"
cons 0
load
cons 0
sub
bEZA "branch_4_rl"
copyI 0
jumpA "block_5"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bEZA "branch_4_rr"
copyI 0
jumpA "block_5"

markA "branch_4_rr"
cons 0
load
cons 4
sub
bEZA "branch_4_ll"
mod
jumpA "block_3"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bEZA "branch_4_lr"
mod
jumpA "block_3"

markA "branch_4_lr"

markA "block_5"
cons 0
load
cons 0
sub
bEZA "branch_5_rl"
cons 1
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
cons 1
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 2
sub
bEZA "branch_5_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bEZA "branch_5_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_5_dr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
div
jumpA "block_4"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bEZA "branch_5_lr"
div
jumpA "block_4"

markA "branch_5_lr"

markA "block_6"
cons 0
load
cons 0
sub
bEZA "branch_6_rl"
cons 1
jumpA "block_7"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bEZA "branch_6_rr"
cons 1
jumpA "block_7"

markA "branch_6_rr"
cons 0
load
cons 2
sub
bEZA "branch_6_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_6_dl"
cons 0
load
cons 3
sub
bEZA "branch_6_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
discard
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
discard
jumpA "block_5"

markA "branch_6_lr"

markA "block_7"
cons 0
load
cons 0
sub
bEZA "branch_7_rl"
sub
jumpA "block_8"

markA "branch_7_rl"
cons 0
load
cons 1
sub
bEZA "branch_7_rr"
sub
jumpA "block_8"

markA "branch_7_rr"
cons 0
load
cons 2
sub
bEZA "branch_7_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_7_dl"
cons 0
load
cons 3
sub
bEZA "branch_7_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_7_dr"
cons 0
load
cons 4
sub
bEZA "branch_7_ll"
discard
jumpA "block_6"

markA "branch_7_ll"
cons 0
load
cons 5
sub
bEZA "branch_7_lr"
discard
jumpA "block_6"

markA "branch_7_lr"

markA "block_8"
cons 0
load
cons 0
sub
bEZA "branch_8_rl"
lgt
jumpA "block_9"

markA "branch_8_rl"
cons 0
load
cons 1
sub
bEZA "branch_8_rr"
lgt
jumpA "block_9"

markA "branch_8_rr"
cons 0
load
cons 2
sub
bEZA "branch_8_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_8_dl"
cons 0
load
cons 3
sub
bEZA "branch_8_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_8_dr"
cons 0
load
cons 4
sub
bEZA "branch_8_ll"
mio outputchar
jumpA "block_7"

markA "branch_8_ll"
cons 0
load
cons 5
sub
bEZA "branch_8_lr"
mio outputchar
jumpA "block_7"

markA "branch_8_lr"

markA "block_9"
cons 0
load
cons 0
sub
bEZA "branch_9_rl"
lnot
jumpA "block_10"

markA "branch_9_rl"
cons 0
load
cons 1
sub
bEZA "branch_9_rr"
lnot
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
jumpA "block_8"

markA "branch_9_dl"
cons 0
load
cons 3
sub
bEZA "branch_9_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_9_dr"
cons 0
load
cons 4
sub
bEZA "branch_9_ll"
lgt
jumpA "block_8"

markA "branch_9_ll"
cons 0
load
cons 5
sub
bEZA "branch_9_lr"
lgt
jumpA "block_8"

markA "branch_9_lr"

markA "block_10"
cons 0
load
cons 0
sub
bEZA "branch_10_rl"
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

markA "branch_10_rl"
cons 0
load
cons 1
sub
bEZA "branch_10_rr"
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

markA "branch_10_rr"
cons 0
load
cons 2
sub
bEZA "branch_10_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_10_dl"
cons 0
load
cons 3
sub
bEZA "branch_10_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_10_dr"
cons 0
load
cons 4
sub
bEZA "branch_10_ll"
halibut
jumpA "block_9"

markA "branch_10_ll"
cons 0
load
cons 5
sub
bEZA "branch_10_lr"
halibut
jumpA "block_9"

markA "branch_10_lr"

markA "block_11"
cons 0
load
cons 0
sub
bEZA "branch_11_rl"
cons 2
jumpA "block_12"

markA "branch_11_rl"
cons 0
load
cons 1
sub
bEZA "branch_11_rr"
cons 2
jumpA "block_12"

markA "branch_11_rr"
cons 0
load
cons 2
sub
bEZA "branch_11_dl"
discard
jumpA "block_31"

markA "branch_11_dl"
cons 0
load
cons 3
sub
bEZA "branch_11_dr"
cons 0
cons 7
store
jumpA "block_8"

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
jumpA "block_10"

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
jumpA "block_10"

markA "branch_11_lr"

markA "block_12"
cons 0
load
cons 0
sub
bEZA "branch_12_rl"
cons 1
jumpA "block_13"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bEZA "branch_12_rr"
cons 1
jumpA "block_13"

markA "branch_12_rr"
cons 0
load
cons 2
sub
bEZA "branch_12_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_12_dl"
cons 0
load
cons 3
sub
bEZA "branch_12_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_12_dr"
cons 0
load
cons 4
sub
bEZA "branch_12_ll"
discard
jumpA "block_11"

markA "branch_12_ll"
cons 0
load
cons 5
sub
bEZA "branch_12_lr"
discard
jumpA "block_11"

markA "branch_12_lr"

markA "block_13"
cons 0
load
cons 0
sub
bEZA "branch_13_rl"
halibut
jumpA "block_14"

markA "branch_13_rl"
cons 0
load
cons 1
sub
bEZA "branch_13_rr"
halibut
jumpA "block_14"

markA "branch_13_rr"
cons 0
load
cons 2
sub
bEZA "branch_13_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_13_dl"
cons 0
load
cons 3
sub
bEZA "branch_13_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_13_dr"
cons 0
load
cons 4
sub
bEZA "branch_13_ll"
discard
jumpA "block_12"

markA "branch_13_ll"
cons 0
load
cons 5
sub
bEZA "branch_13_lr"
discard
jumpA "block_12"

markA "branch_13_lr"

markA "block_14"
cons 0
load
cons 0
sub
bEZA "branch_14_rl"
copyI 0
jumpA "block_15"

markA "branch_14_rl"
cons 0
load
cons 1
sub
bEZA "branch_14_rr"
copyI 0
jumpA "block_15"

markA "branch_14_rr"
cons 0
load
cons 2
sub
bEZA "branch_14_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_14_dl"
cons 0
load
cons 3
sub
bEZA "branch_14_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_14_dr"
cons 0
load
cons 4
sub
bEZA "branch_14_ll"
lnot
jumpA "block_13"

markA "branch_14_ll"
cons 0
load
cons 5
sub
bEZA "branch_14_lr"
lnot
jumpA "block_13"

markA "branch_14_lr"

markA "block_15"
cons 0
load
cons 0
sub
bEZA "branch_15_rl"
cons 3
jumpA "block_16"

markA "branch_15_rl"
cons 0
load
cons 1
sub
bEZA "branch_15_rr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_15_rr"
cons 0
load
cons 2
sub
bEZA "branch_15_dl"
cons 0
cons 6
store
jumpA "block_8"

markA "branch_15_dl"
cons 0
load
cons 3
sub
bEZA "branch_15_dr"
cons 0
cons 7
store
jumpA "block_8"

markA "branch_15_dr"
cons 0
load
cons 4
sub
bEZA "branch_15_ll"
jumpA "block_38"

markA "branch_15_ll"
cons 0
load
cons 5
sub
bEZA "branch_15_lr"
div
jumpA "block_14"

markA "branch_15_lr"

markA "block_16"
cons 0
load
cons 0
sub
bEZA "branch_16_rl"
cons 2
jumpA "block_17"

markA "branch_16_rl"
cons 0
load
cons 1
sub
bEZA "branch_16_rr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_16_rr"
cons 0
load
cons 2
sub
bEZA "branch_16_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_16_dl"
cons 0
load
cons 3
sub
bEZA "branch_16_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_16_dr"
cons 0
load
cons 4
sub
bEZA "branch_16_ll"
discard
jumpA "block_15"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bEZA "branch_16_lr"
discard
jumpA "block_15"

markA "branch_16_lr"

markA "block_17"
cons 0
load
cons 0
sub
bEZA "branch_17_rl"
halibut
jumpA "block_18"

markA "branch_17_rl"
cons 0
load
cons 1
sub
bEZA "branch_17_rr"
halibut
jumpA "block_18"

markA "branch_17_rr"
cons 0
load
cons 2
sub
bEZA "branch_17_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_17_dl"
cons 0
load
cons 3
sub
bEZA "branch_17_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_17_dr"
cons 0
load
cons 4
sub
bEZA "branch_17_ll"
discard
jumpA "block_16"

markA "branch_17_ll"
cons 0
load
cons 5
sub
bEZA "branch_17_lr"
discard
jumpA "block_16"

markA "branch_17_lr"

markA "block_18"
cons 0
load
cons 0
sub
bEZA "branch_18_rl"
jumpA "block_20"

markA "branch_18_rl"
cons 0
load
cons 1
sub
bEZA "branch_18_rr"
jumpA "block_20"

markA "branch_18_rr"
cons 0
load
cons 2
sub
bEZA "branch_18_dl"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_18_dl"
cons 0
load
cons 3
sub
bEZA "branch_18_dr"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_18_dr"
cons 0
load
cons 4
sub
bEZA "branch_18_ll"
lnot
jumpA "block_17"

markA "branch_18_ll"
cons 0
load
cons 5
sub
bEZA "branch_18_lr"
lnot
jumpA "block_17"

markA "branch_18_lr"

markA "block_20"
cons 0
load
cons 0
sub
bEZA "branch_20_rl"
copyI 0
jumpA "block_21"

markA "branch_20_rl"
cons 0
load
cons 1
sub
bEZA "branch_20_rr"
copyI 0
jumpA "block_21"

markA "branch_20_rr"
cons 0
load
cons 2
sub
bEZA "branch_20_dl"
jumpA "block_39"

markA "branch_20_dl"
cons 0
load
cons 3
sub
bEZA "branch_20_dr"
jumpA "block_39"

markA "branch_20_dr"
cons 0
load
cons 4
sub
bEZA "branch_20_ll"
jumpA "block_18"

markA "branch_20_ll"
cons 0
load
cons 5
sub
bEZA "branch_20_lr"
jumpA "block_18"

markA "branch_20_lr"

markA "block_21"
cons 0
load
cons 0
sub
bEZA "branch_21_rl"
cons 1
jumpA "block_22"

markA "branch_21_rl"
cons 0
load
cons 1
sub
bEZA "branch_21_rr"
cons 1
jumpA "block_22"

markA "branch_21_rr"
cons 0
load
cons 2
sub
bEZA "branch_21_dl"
jumpA "block_39"

markA "branch_21_dl"
cons 0
load
cons 3
sub
bEZA "branch_21_dr"
jumpA "block_39"

markA "branch_21_dr"
cons 0
load
cons 4
sub
bEZA "branch_21_ll"
div
jumpA "block_20"

markA "branch_21_ll"
cons 0
load
cons 5
sub
bEZA "branch_21_lr"
div
jumpA "block_20"

markA "branch_21_lr"

markA "block_22"
cons 0
load
cons 0
sub
bEZA "branch_22_rl"
lgt
jumpA "block_23"

markA "branch_22_rl"
cons 0
load
cons 1
sub
bEZA "branch_22_rr"
lgt
jumpA "block_23"

markA "branch_22_rr"
cons 0
load
cons 2
sub
bEZA "branch_22_dl"
mio inputdec
jumpA "block_33"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bEZA "branch_22_dr"
mio inputdec
jumpA "block_33"

markA "branch_22_dr"
cons 0
load
cons 4
sub
bEZA "branch_22_ll"
discard
jumpA "block_21"

markA "branch_22_ll"
cons 0
load
cons 5
sub
bEZA "branch_22_lr"
discard
jumpA "block_21"

markA "branch_22_lr"

markA "block_23"
cons 0
load
cons 0
sub
bEZA "branch_23_rl"
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
jumpA "block_24"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bEZA "branch_23_rr"
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
jumpA "block_24"

markA "branch_23_rr"
cons 0
load
cons 2
sub
bEZA "branch_23_dl"
mul
jumpA "block_33"

markA "branch_23_dl"
cons 0
load
cons 3
sub
bEZA "branch_23_dr"
mul
jumpA "block_33"

markA "branch_23_dr"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
lgt
jumpA "block_22"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
lgt
jumpA "block_22"

markA "branch_23_lr"

markA "block_24"
cons 0
load
cons 0
sub
bEZA "branch_24_rl"
jumpA "block_26"

markA "branch_24_rl"
cons 0
load
cons 1
sub
bEZA "branch_24_rr"
jumpA "block_26"

markA "branch_24_rr"
cons 0
load
cons 2
sub
bEZA "branch_24_dl"
cons 1
jumpA "block_34"

markA "branch_24_dl"
cons 0
load
cons 3
sub
bEZA "branch_24_dr"
cons 1
jumpA "block_34"

markA "branch_24_dr"
cons 0
load
cons 4
sub
bEZA "branch_24_ll"
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
jumpA "block_23"

markA "branch_24_ll"
cons 0
load
cons 5
sub
bEZA "branch_24_lr"
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
jumpA "block_23"

markA "branch_24_lr"

markA "block_26"
cons 0
load
cons 0
sub
bEZA "branch_26_rl"
cons 3
jumpA "block_27"

markA "branch_26_rl"
cons 0
load
cons 4
sub
bEZA "branch_26_ll"
jumpA "block_40"

markA "branch_26_ll"
cons 0
load
cons 5
sub
bEZA "branch_26_lr"
jumpA "block_24"

markA "branch_26_lr"

markA "block_27"
cons 0
load
cons 0
sub
bEZA "branch_27_rl"
cons 2
jumpA "block_28"

markA "branch_27_rl"
cons 0
load
cons 1
sub
bEZA "branch_27_rr"
jumpA "block_36"

markA "branch_27_rr"
cons 0
load
cons 4
sub
bEZA "branch_27_ll"
discard
jumpA "block_26"

markA "branch_27_ll"
cons 0
load
cons 5
sub
bEZA "branch_27_lr"
discard
jumpA "block_26"

markA "branch_27_lr"

markA "block_28"
cons 0
load
cons 0
sub
bEZA "branch_28_rl"
halibut
jumpA "block_29"

markA "branch_28_rl"
cons 0
load
cons 1
sub
bEZA "branch_28_rr"
halibut
jumpA "block_29"

markA "branch_28_rr"
cons 0
load
cons 2
sub
bEZA "branch_28_dl"
cons 0
cons 5
store
jumpA "block_27"

markA "branch_28_dl"
cons 0
load
cons 3
sub
bEZA "branch_28_dr"
cons 0
cons 4
store
jumpA "block_27"

markA "branch_28_dr"
cons 0
load
cons 4
sub
bEZA "branch_28_ll"
discard
jumpA "block_27"

markA "branch_28_ll"
cons 0
load
cons 5
sub
bEZA "branch_28_lr"
discard
jumpA "block_27"

markA "branch_28_lr"

markA "block_29"
cons 0
load
cons 2
sub
bEZA "branch_29_dl"
discard
jumpA "block_36"

markA "branch_29_dl"
cons 0
load
cons 3
sub
bEZA "branch_29_dr"
discard
jumpA "block_36"

markA "branch_29_dr"
cons 0
load
cons 4
sub
bEZA "branch_29_ll"
lnot
jumpA "block_28"

markA "branch_29_ll"
cons 0
load
cons 5
sub
bEZA "branch_29_lr"
lnot
jumpA "block_28"

markA "branch_29_lr"

markA "block_31"
cons 0
load
cons 0
sub
bEZA "branch_31_rl"
jumpA "block_15"

markA "branch_31_rl"
cons 0
load
cons 1
sub
bEZA "branch_31_rr"
jumpA "block_15"

markA "branch_31_rr"
cons 0
load
cons 2
sub
bEZA "branch_31_dl"
discard
jumpA "block_38"

markA "branch_31_dl"
cons 0
load
cons 3
sub
bEZA "branch_31_dr"
discard
jumpA "block_38"

markA "branch_31_dr"
cons 0
load
cons 4
sub
bEZA "branch_31_ll"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_31_ll"
cons 0
load
cons 5
sub
bEZA "branch_31_lr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_31_lr"
cons 0
load
cons 6
sub
bEZA "branch_31_ul"
cons 1
jumpA "block_11"

markA "branch_31_ul"
cons 0
load
cons 7
sub
bEZA "branch_31_ur"
cons 1
jumpA "block_11"

markA "branch_31_ur"

markA "block_33"
cons 0
load
cons 0
sub
bEZA "branch_33_rl"
div
jumpA "block_34"

markA "branch_33_rl"
cons 0
load
cons 1
sub
bEZA "branch_33_rr"
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
jumpA "block_40"

markA "branch_33_rr"
cons 0
load
cons 2
sub
bEZA "branch_33_dl"
jumpA "block_62"

markA "branch_33_dl"
cons 0
load
cons 3
sub
bEZA "branch_33_dr"
jumpA "block_46"

markA "branch_33_dr"
cons 0
load
cons 4
sub
bEZA "branch_33_ll"
div
jumpA "block_39"

markA "branch_33_ll"
cons 0
load
cons 5
sub
bEZA "branch_33_lr"
cons 0
cons 6
store
jumpA "block_20"

markA "branch_33_lr"
cons 0
load
cons 6
sub
bEZA "branch_33_ul"
mod
jumpA "block_22"

markA "branch_33_ul"
cons 0
load
cons 7
sub
bEZA "branch_33_ur"
mio outputdec
jumpA "block_23"

markA "branch_33_ur"

markA "block_34"
cons 0
load
cons 0
sub
bEZA "branch_34_rl"
jumpA "block_26"

markA "branch_34_rl"
cons 0
load
cons 1
sub
bEZA "branch_34_rr"
jumpA "block_26"

markA "branch_34_rr"
cons 0
load
cons 2
sub
bEZA "branch_34_dl"
sub
jumpA "block_40"

markA "branch_34_dl"
cons 0
load
cons 3
sub
bEZA "branch_34_dr"
sub
jumpA "block_40"

markA "branch_34_dr"
cons 0
load
cons 4
sub
bEZA "branch_34_ll"
copyI 0
jumpA "block_33"

markA "branch_34_ll"
cons 0
load
cons 5
sub
bEZA "branch_34_lr"
copyI 0
jumpA "block_33"

markA "branch_34_lr"
cons 0
load
cons 6
sub
bEZA "branch_34_ul"
discard
jumpA "block_24"

markA "branch_34_ul"
cons 0
load
cons 7
sub
bEZA "branch_34_ur"
discard
jumpA "block_24"

markA "branch_34_ur"

markA "block_36"
cons 0
load
cons 2
sub
bEZA "branch_36_dl"
discard
jumpA "block_41"

markA "branch_36_dl"
cons 0
load
cons 3
sub
bEZA "branch_36_dr"
discard
jumpA "block_41"

markA "branch_36_dr"
cons 0
load
cons 4
sub
bEZA "branch_36_ll"
jumpA "block_27"

markA "branch_36_ll"
cons 0
load
cons 5
sub
bEZA "branch_36_lr"
jumpA "block_27"

markA "branch_36_lr"
cons 0
load
cons 6
sub
bEZA "branch_36_ul"
cons 1
jumpA "block_29"

markA "branch_36_ul"
cons 0
load
cons 7
sub
bEZA "branch_36_ur"
cons 1
jumpA "block_29"

markA "branch_36_ur"

markA "block_37"
cons 0
load
cons 6
sub
bEZA "branch_37_ul"
add
jumpA "block_0"

markA "branch_37_ul"
cons 0
load
cons 7
sub
bEZA "branch_37_ur"
add
jumpA "block_0"

markA "branch_37_ur"

markA "block_38"
cons 0
load
cons 0
sub
bEZA "branch_38_rl"
jumpA "block_15"

markA "branch_38_rl"
cons 0
load
cons 1
sub
bEZA "branch_38_rr"
jumpA "block_15"

markA "branch_38_rr"
cons 0
load
cons 2
sub
bEZA "branch_38_dl"
cons 1
jumpA "block_42"

markA "branch_38_dl"
cons 0
load
cons 3
sub
bEZA "branch_38_dr"
cons 1
jumpA "block_42"

markA "branch_38_dr"
cons 0
load
cons 4
sub
bEZA "branch_38_ll"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_38_ll"
cons 0
load
cons 5
sub
bEZA "branch_38_lr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_38_lr"
cons 0
load
cons 6
sub
bEZA "branch_38_ul"
cons 1
jumpA "block_31"

markA "branch_38_ul"
cons 0
load
cons 7
sub
bEZA "branch_38_ur"
cons 1
jumpA "block_31"

markA "branch_38_ur"

markA "block_39"
cons 0
load
cons 0
sub
bEZA "branch_39_rl"
copyI 0
jumpA "block_33"

markA "branch_39_rl"
cons 0
load
cons 1
sub
bEZA "branch_39_rr"
copyI 0
jumpA "block_33"

markA "branch_39_rr"
cons 0
load
cons 2
sub
bEZA "branch_39_dl"
lnot
jumpA "block_44"

markA "branch_39_dl"
cons 0
load
cons 3
sub
bEZA "branch_39_dr"
cons 0
cons 7
store
jumpA "block_39"

markA "branch_39_dr"
cons 0
load
cons 6
sub
bEZA "branch_39_ul"
jumpA "block_20"

markA "branch_39_ul"
cons 0
load
cons 7
sub
bEZA "branch_39_ur"
jumpA "block_21"

markA "branch_39_ur"

markA "block_40"
cons 0
load
cons 0
sub
bEZA "branch_40_rl"
jumpA "block_26"

markA "branch_40_rl"
cons 0
load
cons 2
sub
bEZA "branch_40_dl"
cons 3
jumpA "block_49"

markA "branch_40_dl"
cons 0
load
cons 3
sub
bEZA "branch_40_dr"
cons 3
jumpA "block_49"

markA "branch_40_dr"
cons 0
load
cons 4
sub
bEZA "branch_40_ll"
jumpA "block_46"

markA "branch_40_ll"
cons 0
load
cons 5
sub
bEZA "branch_40_lr"
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
jumpA "block_33"

markA "branch_40_lr"
cons 0
load
cons 6
sub
bEZA "branch_40_ul"
mio outputchar
jumpA "block_34"

markA "branch_40_ul"
cons 0
load
cons 7
sub
bEZA "branch_40_ur"
mio outputchar
jumpA "block_34"

markA "branch_40_ur"

markA "block_41"
cons 0
load
cons 2
sub
bEZA "branch_41_dl"
mio outputdec
jumpA "block_58"

markA "branch_41_dl"
cons 0
load
cons 3
sub
bEZA "branch_41_dr"
mio outputdec
jumpA "block_58"

markA "branch_41_dr"
cons 0
load
cons 6
sub
bEZA "branch_41_ul"
cons 12
jumpA "block_36"

markA "branch_41_ul"
cons 0
load
cons 7
sub
bEZA "branch_41_ur"
cons 12
jumpA "block_36"

markA "branch_41_ur"

markA "block_42"
cons 0
load
cons 0
sub
bEZA "branch_42_rl"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_42_rl"
cons 0
load
cons 1
sub
bEZA "branch_42_rr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_42_rr"
cons 0
load
cons 2
sub
bEZA "branch_42_dl"
jumpA "block_52"

markA "branch_42_dl"
cons 0
load
cons 3
sub
bEZA "branch_42_dr"
jumpA "block_52"

markA "branch_42_dr"
cons 0
load
cons 4
sub
bEZA "branch_42_ll"
cons 0
cons 7
store
jumpA "block_5"

markA "branch_42_ll"
cons 0
load
cons 5
sub
bEZA "branch_42_lr"
cons 0
cons 6
store
jumpA "block_5"

markA "branch_42_lr"
cons 0
load
cons 6
sub
bEZA "branch_42_ul"
discard
jumpA "block_38"

markA "branch_42_ul"
cons 0
load
cons 7
sub
bEZA "branch_42_ur"
discard
jumpA "block_38"

markA "branch_42_ur"

markA "block_44"
cons 0
load
cons 0
sub
bEZA "branch_44_rl"
jumpA "block_40"

markA "branch_44_rl"
cons 0
load
cons 1
sub
bEZA "branch_44_rr"
jumpA "block_40"

markA "branch_44_rr"
cons 0
load
cons 2
sub
bEZA "branch_44_dl"
discard
jumpA "block_46"

markA "branch_44_dl"
cons 0
load
cons 3
sub
bEZA "branch_44_dr"
discard
jumpA "block_46"

markA "branch_44_dr"
cons 0
load
cons 4
sub
bEZA "branch_44_ll"
cons 0
cons 7
store
jumpA "block_39"

markA "branch_44_ll"
cons 0
load
cons 5
sub
bEZA "branch_44_lr"
cons 0
cons 6
store
jumpA "block_39"

markA "branch_44_lr"
cons 0
load
cons 6
sub
bEZA "branch_44_ul"
halibut
jumpA "block_39"

markA "branch_44_ul"
cons 0
load
cons 7
sub
bEZA "branch_44_ur"
halibut
jumpA "block_39"

markA "branch_44_ur"

markA "block_46"
cons 0
load
cons 0
sub
bEZA "branch_46_rl"
jumpA "block_40"

markA "branch_46_rl"
cons 0
load
cons 1
sub
bEZA "branch_46_rr"
jumpA "block_40"

markA "branch_46_rr"
cons 0
load
cons 2
sub
bEZA "branch_46_dl"
jumpA "block_48"

markA "branch_46_dl"
cons 0
load
cons 3
sub
bEZA "branch_46_dr"
discard
jumpA "block_48"

markA "branch_46_dr"
cons 0
load
cons 4
sub
bEZA "branch_46_ll"
cons 0
cons 7
store
jumpA "block_39"

markA "branch_46_ll"
cons 0
load
cons 5
sub
bEZA "branch_46_lr"
cons 0
cons 6
store
jumpA "block_39"

markA "branch_46_lr"
cons 0
load
cons 6
sub
bEZA "branch_46_ul"
cons 2
jumpA "block_44"

markA "branch_46_ul"
cons 0
load
cons 7
sub
bEZA "branch_46_ur"
jumpA "block_33"

markA "branch_46_ur"

markA "block_48"
cons 0
load
cons 0
sub
bEZA "branch_48_rl"
jumpA "block_49"

markA "branch_48_rl"
cons 0
load
cons 1
sub
bEZA "branch_48_rr"
jumpA "block_49"

markA "branch_48_rr"
cons 0
load
cons 2
sub
bEZA "branch_48_dl"
cons 3
jumpA "block_55"

markA "branch_48_dl"
cons 0
load
cons 3
sub
bEZA "branch_48_dr"
mio outputdec
jumpA "block_54"

markA "branch_48_dr"
cons 0
load
cons 4
sub
bEZA "branch_48_ll"
cons 0
cons 7
store
jumpA "block_39"

markA "branch_48_ll"
cons 0
load
cons 5
sub
bEZA "branch_48_lr"
cons 0
cons 6
store
jumpA "block_39"

markA "branch_48_lr"
cons 0
load
cons 6
sub
bEZA "branch_48_ul"
cons 3
jumpA "block_46"

markA "branch_48_ul"
cons 0
load
cons 7
sub
bEZA "branch_48_ur"
cons 3
jumpA "block_46"

markA "branch_48_ur"

markA "block_49"
cons 0
load
cons 2
sub
bEZA "branch_49_dl"
cons 2
jumpA "block_56"

markA "branch_49_dl"
cons 0
load
cons 3
sub
bEZA "branch_49_dr"
cons 2
jumpA "block_56"

markA "branch_49_dr"
cons 0
load
cons 4
sub
bEZA "branch_49_ll"
jumpA "block_48"

markA "branch_49_ll"
cons 0
load
cons 5
sub
bEZA "branch_49_lr"
jumpA "block_48"

markA "branch_49_lr"
cons 0
load
cons 6
sub
bEZA "branch_49_ul"
discard
jumpA "block_40"

markA "branch_49_ul"
cons 0
load
cons 7
sub
bEZA "branch_49_ur"
discard
jumpA "block_40"

markA "branch_49_ur"

markA "block_51"

markA "block_52"
cons 0
load
cons 2
sub
bEZA "branch_52_dl"
mio outputdec
jumpA "block_51"

markA "branch_52_dl"
cons 0
load
cons 3
sub
bEZA "branch_52_dr"
mio outputdec
jumpA "block_51"

markA "branch_52_dr"
cons 0
load
cons 6
sub
bEZA "branch_52_ul"
jumpA "block_42"

markA "branch_52_ul"
cons 0
load
cons 7
sub
bEZA "branch_52_ur"
jumpA "block_42"

markA "branch_52_ur"

markA "block_54"
cons 0
load
cons 0
sub
bEZA "branch_54_rl"
add
jumpA "block_55"

markA "branch_54_rl"
cons 0
load
cons 1
sub
bEZA "branch_54_rr"
add
jumpA "block_55"

markA "branch_54_rr"
cons 0
load
cons 2
sub
bEZA "branch_54_dl"
lnot
jumpA "block_60"

markA "branch_54_dl"
cons 0
load
cons 3
sub
bEZA "branch_54_dr"
lnot
jumpA "block_60"

markA "branch_54_dr"
cons 0
load
cons 4
sub
bEZA "branch_54_ll"
cons 0
cons 7
store
jumpA "block_39"

markA "branch_54_ll"
cons 0
load
cons 5
sub
bEZA "branch_54_lr"
cons 0
cons 6
store
jumpA "block_39"

markA "branch_54_lr"
cons 0
load
cons 6
sub
bEZA "branch_54_ul"
mul
jumpA "block_48"

markA "branch_54_ul"
cons 0
load
cons 7
sub
bEZA "branch_54_ur"
mul
jumpA "block_48"

markA "branch_54_ur"

markA "block_55"
cons 0
load
cons 0
sub
bEZA "branch_55_rl"
jumpA "block_56"

markA "branch_55_rl"
cons 0
load
cons 1
sub
bEZA "branch_55_rr"
jumpA "block_56"

markA "branch_55_rr"
cons 0
load
cons 2
sub
bEZA "branch_55_dl"
sub
jumpA "block_61"

markA "branch_55_dl"
cons 0
load
cons 3
sub
bEZA "branch_55_dr"
sub
jumpA "block_61"

markA "branch_55_dr"
cons 0
load
cons 4
sub
bEZA "branch_55_ll"
mio inputchar
jumpA "block_54"

markA "branch_55_ll"
cons 0
load
cons 5
sub
bEZA "branch_55_lr"
mio inputchar
jumpA "block_54"

markA "branch_55_lr"
cons 0
load
cons 6
sub
bEZA "branch_55_ul"
discard
jumpA "block_48"

markA "branch_55_ul"
cons 0
load
cons 7
sub
bEZA "branch_55_ur"
discard
jumpA "block_48"

markA "branch_55_ur"

markA "block_56"
cons 0
load
cons 2
sub
bEZA "branch_56_dl"
halibut
jumpA "block_63"

markA "branch_56_dl"
cons 0
load
cons 3
sub
bEZA "branch_56_dr"
halibut
jumpA "block_63"

markA "branch_56_dr"
cons 0
load
cons 4
sub
bEZA "branch_56_ll"
jumpA "block_55"

markA "branch_56_ll"
cons 0
load
cons 5
sub
bEZA "branch_56_lr"
jumpA "block_55"

markA "branch_56_lr"
cons 0
load
cons 6
sub
bEZA "branch_56_ul"
discard
jumpA "block_49"

markA "branch_56_ul"
cons 0
load
cons 7
sub
bEZA "branch_56_ur"
discard
jumpA "block_49"

markA "branch_56_ur"

markA "block_58"

markA "block_60"
cons 0
load
cons 0
sub
bEZA "branch_60_rl"
discard
jumpA "block_61"

markA "branch_60_rl"
cons 0
load
cons 1
sub
bEZA "branch_60_rr"
discard
jumpA "block_61"

markA "branch_60_rr"
cons 0
load
cons 6
sub
bEZA "branch_60_ul"
halibut
jumpA "block_54"

markA "branch_60_ul"
cons 0
load
cons 7
sub
bEZA "branch_60_ur"
halibut
jumpA "block_54"

markA "branch_60_ur"

markA "block_61"
cons 0
load
cons 0
sub
bEZA "branch_61_rl"
discard
jumpA "block_62"

markA "branch_61_rl"
cons 0
load
cons 1
sub
bEZA "branch_61_rr"
discard
jumpA "block_62"

markA "branch_61_rr"
cons 0
load
cons 5
sub
bEZA "branch_61_lr"
cons 3
jumpA "block_60"

markA "branch_61_lr"
cons 0
load
cons 6
sub
bEZA "branch_61_ul"
mio outputchar
jumpA "block_55"

markA "branch_61_ul"
cons 0
load
cons 7
sub
bEZA "branch_61_ur"
mio outputchar
jumpA "block_55"

markA "branch_61_ur"

markA "block_62"
cons 0
load
cons 0
sub
bEZA "branch_62_rl"
div
jumpA "block_63"

markA "branch_62_rl"
cons 0
load
cons 5
sub
bEZA "branch_62_lr"
cons 4
jumpA "block_61"

markA "branch_62_lr"
cons 0
load
cons 6
sub
bEZA "branch_62_ul"
jumpA "block_33"

markA "branch_62_ul"
cons 0
load
cons 7
sub
bEZA "branch_62_ur"
jumpA "block_33"

markA "branch_62_ur"

markA "block_63"
cons 0
load
cons 4
sub
bEZA "branch_63_ll"
copyI 0
jumpA "block_62"

markA "branch_63_ll"
cons 0
load
cons 5
sub
bEZA "branch_63_lr"
copyI 0
jumpA "block_62"

markA "branch_63_lr"
cons 0
load
cons 6
sub
bEZA "branch_63_ul"
lnot
jumpA "block_56"

markA "branch_63_ul"
cons 0
load
cons 7
sub
bEZA "branch_63_ur"
lnot
jumpA "block_56"

markA "branch_63_ur"
end
