cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 72
jumpA "block_1"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 72
jumpA "block_1"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
div
jumpA "block_16"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bEZA "branch_0_dr"
div
jumpA "block_16"

markA "branch_0_dr"

markA "block_1"
cons 0
load
cons 0
sub
bEZA "branch_1_rl"
mio outputchar
jumpA "block_2"

markA "branch_1_rl"
cons 0
load
cons 1
sub
bEZA "branch_1_rr"
mio outputchar
jumpA "block_2"

markA "branch_1_rr"
cons 0
load
cons 2
sub
bEZA "branch_1_dl"
mio outputchar
jumpA "block_2"

markA "branch_1_dl"
cons 0
load
cons 3
sub
bEZA "branch_1_dr"
mio outputchar
jumpA "block_2"

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
cons 101
jumpA "block_3"

markA "branch_2_rl"
cons 0
load
cons 1
sub
bEZA "branch_2_rr"
mio inputchar
jumpA "block_4"

markA "branch_2_rr"
cons 0
load
cons 2
sub
bEZA "branch_2_dl"
lgt
jumpA "block_16"

markA "branch_2_dl"
cons 0
load
cons 3
sub
bEZA "branch_2_dr"
add
jumpA "block_0"

markA "branch_2_dr"
cons 0
load
cons 4
sub
bEZA "branch_2_ll"
add
jumpA "block_0"

markA "branch_2_ll"
cons 0
load
cons 5
sub
bEZA "branch_2_lr"
add
jumpA "block_0"

markA "branch_2_lr"

markA "block_3"
cons 0
load
cons 0
sub
bEZA "branch_3_rl"
mio outputchar
jumpA "block_4"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bEZA "branch_3_rr"
mio outputchar
jumpA "block_4"

markA "branch_3_rr"
cons 0
load
cons 2
sub
bEZA "branch_3_dl"
mio outputchar
jumpA "block_4"

markA "branch_3_dl"
cons 0
load
cons 3
sub
bEZA "branch_3_dr"
mio outputchar
jumpA "block_4"

markA "branch_3_dr"
cons 0
load
cons 4
sub
bEZA "branch_3_ll"
discard
jumpA "block_2"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bEZA "branch_3_lr"
discard
jumpA "block_2"

markA "branch_3_lr"

markA "block_4"
cons 0
load
cons 2
sub
bEZA "branch_4_dl"
cons 108
jumpA "block_20"

markA "branch_4_dl"
cons 0
load
cons 3
sub
bEZA "branch_4_dr"
cons 108
jumpA "block_20"

markA "branch_4_dr"
cons 0
load
cons 4
sub
bEZA "branch_4_ll"
add
jumpA "block_2"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bEZA "branch_4_lr"
add
jumpA "block_2"

markA "branch_4_lr"

markA "block_8"

markA "block_13"
cons 0
load
cons 2
sub
bEZA "branch_13_dl"
discard
jumpA "block_15"

markA "branch_13_dl"
cons 0
load
cons 3
sub
bEZA "branch_13_dr"
discard
jumpA "block_15"

markA "branch_13_dr"
cons 0
load
cons 6
sub
bEZA "branch_13_ul"
mio outputchar
jumpA "block_8"

markA "branch_13_ul"
cons 0
load
cons 7
sub
bEZA "branch_13_ur"
mio outputchar
jumpA "block_8"

markA "branch_13_ur"

markA "block_15"
cons 0
load
cons 1
sub
bEZA "branch_15_rr"
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
jumpA "block_26"

markA "branch_15_rr"
cons 0
load
cons 2
sub
bEZA "branch_15_dl"
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
jumpA "block_28"

markA "branch_15_dl"
cons 0
load
cons 3
sub
bEZA "branch_15_dr"
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
jumpA "block_28"

markA "branch_15_dr"
cons 0
load
cons 6
sub
bEZA "branch_15_ul"
mio inputchar
jumpA "block_0"

markA "branch_15_ul"
cons 0
load
cons 7
sub
bEZA "branch_15_ur"
cons 33
jumpA "block_13"

markA "branch_15_ur"

markA "block_16"
cons 0
load
cons 4
sub
bEZA "branch_16_ll"
cons 100
jumpA "block_22"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bEZA "branch_16_lr"
mio inputchar
jumpA "block_15"

markA "branch_16_lr"
cons 0
load
cons 6
sub
bEZA "branch_16_ul"
lgt
jumpA "block_2"

markA "branch_16_ul"
cons 0
load
cons 7
sub
bEZA "branch_16_ur"
lgt
jumpA "block_2"

markA "branch_16_ur"

markA "block_18"
cons 0
load
cons 2
sub
bEZA "branch_18_dl"
cons 111
jumpA "block_35"

markA "branch_18_dl"
cons 0
load
cons 3
sub
bEZA "branch_18_dr"
cons 111
jumpA "block_35"

markA "branch_18_dr"
cons 0
load
cons 4
sub
bEZA "branch_18_ll"
halibut
jumpA "block_16"

markA "branch_18_ll"
cons 0
load
cons 6
sub
bEZA "branch_18_ul"
cons 111
jumpA "block_4"

markA "branch_18_ul"
cons 0
load
cons 7
sub
bEZA "branch_18_ur"
cons 111
jumpA "block_4"

markA "branch_18_ur"

markA "block_20"
cons 0
load
cons 2
sub
bEZA "branch_20_dl"
copyI 0
jumpA "block_21"

markA "branch_20_dl"
cons 0
load
cons 3
sub
bEZA "branch_20_dr"
copyI 0
jumpA "block_21"

markA "branch_20_dr"
cons 0
load
cons 4
sub
bEZA "branch_20_ll"
cons 2
jumpA "block_18"

markA "branch_20_ll"
cons 0
load
cons 5
sub
bEZA "branch_20_lr"
cons 2
jumpA "block_18"

markA "branch_20_lr"
cons 0
load
cons 6
sub
bEZA "branch_20_ul"
cons 2
jumpA "block_18"

markA "branch_20_ul"
cons 0
load
cons 7
sub
bEZA "branch_20_ur"
discard
jumpA "block_4"

markA "branch_20_ur"

markA "block_21"
cons 0
load
cons 2
sub
bEZA "branch_21_dl"
mod
jumpA "block_18"

markA "branch_21_dl"
cons 0
load
cons 3
sub
bEZA "branch_21_dr"
copyI 0
jumpA "block_23"

markA "branch_21_dr"
cons 0
load
cons 4
sub
bEZA "branch_21_ll"
mod
jumpA "block_18"

markA "branch_21_ll"
cons 0
load
cons 5
sub
bEZA "branch_21_lr"
mod
jumpA "block_18"

markA "branch_21_lr"
cons 0
load
cons 6
sub
bEZA "branch_21_ul"
div
jumpA "block_20"

markA "branch_21_ul"
cons 0
load
cons 7
sub
bEZA "branch_21_ur"
div
jumpA "block_20"

markA "branch_21_ur"

markA "block_22"
cons 0
load
cons 0
sub
bEZA "branch_22_rl"
discard
jumpA "block_16"

markA "branch_22_rl"
cons 0
load
cons 1
sub
bEZA "branch_22_rr"
discard
jumpA "block_16"

markA "branch_22_rr"
cons 0
load
cons 2
sub
bEZA "branch_22_dl"
mio outputchar
jumpA "block_15"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bEZA "branch_22_dr"
mio outputchar
jumpA "block_15"

markA "branch_22_dr"
cons 0
load
cons 4
sub
bEZA "branch_22_ll"
mio outputchar
jumpA "block_15"

markA "branch_22_ll"
cons 0
load
cons 5
sub
bEZA "branch_22_lr"
mio outputchar
jumpA "block_15"

markA "branch_22_lr"
cons 0
load
cons 6
sub
bEZA "branch_22_ul"
mio outputchar
jumpA "block_15"

markA "branch_22_ul"
cons 0
load
cons 7
sub
bEZA "branch_22_ur"
mio outputchar
jumpA "block_15"

markA "branch_22_ur"

markA "block_23"
cons 0
load
cons 0
sub
bEZA "branch_23_rl"
halibut
jumpA "block_18"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bEZA "branch_23_rr"
halibut
jumpA "block_18"

markA "branch_23_rr"
cons 0
load
cons 2
sub
bEZA "branch_23_dl"
mio outputchar
jumpA "block_25"

markA "branch_23_dl"
cons 0
load
cons 3
sub
bEZA "branch_23_dr"
mio outputchar
jumpA "block_25"

markA "branch_23_dr"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
halibut
jumpA "block_18"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
halibut
jumpA "block_18"

markA "branch_23_lr"
cons 0
load
cons 6
sub
bEZA "branch_23_ul"
div
jumpA "block_21"

markA "branch_23_ul"
cons 0
load
cons 7
sub
bEZA "branch_23_ur"
div
jumpA "block_21"

markA "branch_23_ur"

markA "block_25"
cons 0
load
cons 0
sub
bEZA "branch_25_rl"
mio outputchar
jumpA "block_18"

markA "branch_25_rl"
cons 0
load
cons 1
sub
bEZA "branch_25_rr"
mio outputchar
jumpA "block_18"

markA "branch_25_rr"
cons 0
load
cons 2
sub
bEZA "branch_25_dl"
mio outputchar
jumpA "block_18"

markA "branch_25_dl"
cons 0
load
cons 3
sub
bEZA "branch_25_dr"
mio outputchar
jumpA "block_18"

markA "branch_25_dr"
cons 0
load
cons 4
sub
bEZA "branch_25_ll"
mio outputchar
jumpA "block_18"

markA "branch_25_ll"
cons 0
load
cons 5
sub
bEZA "branch_25_lr"
mio outputchar
jumpA "block_18"

markA "branch_25_lr"
cons 0
load
cons 6
sub
bEZA "branch_25_ul"
sub
jumpA "block_23"

markA "branch_25_ul"
cons 0
load
cons 7
sub
bEZA "branch_25_ur"
sub
jumpA "block_23"

markA "branch_25_ur"

markA "block_26"
cons 0
load
cons 0
sub
bEZA "branch_26_rl"
mio outputchar
jumpA "block_27"

markA "branch_26_rl"
cons 0
load
cons 1
sub
bEZA "branch_26_rr"
mio outputchar
jumpA "block_27"

markA "branch_26_rr"
cons 0
load
cons 2
sub
bEZA "branch_26_dl"
discard
jumpA "block_28"

markA "branch_26_dl"
cons 0
load
cons 3
sub
bEZA "branch_26_dr"
discard
jumpA "block_28"

markA "branch_26_dr"
cons 0
load
cons 4
sub
bEZA "branch_26_ll"
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
jumpA "block_15"

markA "branch_26_ll"
cons 0
load
cons 5
sub
bEZA "branch_26_lr"
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
jumpA "block_15"

markA "branch_26_lr"

markA "block_27"
cons 0
load
cons 0
sub
bEZA "branch_27_rl"
mio outputchar
jumpA "block_16"

markA "branch_27_rl"
cons 0
load
cons 1
sub
bEZA "branch_27_rr"
mio outputchar
jumpA "block_16"

markA "branch_27_rr"
cons 0
load
cons 2
sub
bEZA "branch_27_dl"
mio outputchar
jumpA "block_16"

markA "branch_27_dl"
cons 0
load
cons 3
sub
bEZA "branch_27_dr"
mio outputchar
jumpA "block_16"

markA "branch_27_dr"
cons 0
load
cons 4
sub
bEZA "branch_27_ll"
sub
jumpA "block_26"

markA "branch_27_ll"
cons 0
load
cons 5
sub
bEZA "branch_27_lr"
sub
jumpA "block_26"

markA "branch_27_lr"
cons 0
load
cons 6
sub
bEZA "branch_27_ul"
mio outputchar
jumpA "block_16"

markA "branch_27_ul"
cons 0
load
cons 7
sub
bEZA "branch_27_ur"
mio outputchar
jumpA "block_16"

markA "branch_27_ur"

markA "block_28"
cons 0
load
cons 0
sub
bEZA "branch_28_rl"
mio inputdec
jumpA "block_16"

markA "branch_28_rl"
cons 0
load
cons 1
sub
bEZA "branch_28_rr"
lnot
jumpA "block_33"

markA "branch_28_rr"
cons 0
load
cons 6
sub
bEZA "branch_28_ul"
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
jumpA "block_15"

markA "branch_28_ul"
cons 0
load
cons 7
sub
bEZA "branch_28_ur"
cons 114
jumpA "block_26"

markA "branch_28_ur"

markA "block_31"
cons 0
load
cons 0
sub
bEZA "branch_31_rl"
copyI 0
jumpA "block_18"

markA "branch_31_rl"
cons 0
load
cons 1
sub
bEZA "branch_31_rr"
sub
jumpA "block_34"

markA "branch_31_rr"
cons 0
load
cons 4
sub
bEZA "branch_31_ll"
cons 119
jumpA "block_33"

markA "branch_31_ll"
cons 0
load
cons 5
sub
bEZA "branch_31_lr"
cons 119
jumpA "block_33"

markA "branch_31_lr"
cons 0
load
cons 6
sub
bEZA "branch_31_ul"
mod
jumpA "block_16"

markA "branch_31_ul"

markA "block_33"
cons 0
load
cons 0
sub
bEZA "branch_33_rl"
discard
jumpA "block_31"

markA "branch_33_rl"
cons 0
load
cons 1
sub
bEZA "branch_33_rr"
discard
jumpA "block_31"

markA "branch_33_rr"
cons 0
load
cons 4
sub
bEZA "branch_33_ll"
mio outputchar
jumpA "block_38"

markA "branch_33_ll"
cons 0
load
cons 5
sub
bEZA "branch_33_lr"
halibut
jumpA "block_28"

markA "branch_33_lr"

markA "block_34"
cons 0
load
cons 0
sub
bEZA "branch_34_rl"
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
jumpA "block_18"

markA "branch_34_rl"
cons 0
load
cons 1
sub
bEZA "branch_34_rr"
add
jumpA "block_36"

markA "branch_34_rr"
cons 0
load
cons 4
sub
bEZA "branch_34_ll"
mio outputchar
jumpA "block_31"

markA "branch_34_ll"
cons 0
load
cons 5
sub
bEZA "branch_34_lr"
mio outputchar
jumpA "block_31"

markA "branch_34_lr"
cons 0
load
cons 6
sub
bEZA "branch_34_ul"
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
jumpA "block_18"

markA "branch_34_ul"
cons 0
load
cons 7
sub
bEZA "branch_34_ur"
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
jumpA "block_18"

markA "branch_34_ur"

markA "block_35"
cons 0
load
cons 2
sub
bEZA "branch_35_dl"
copyI 0
jumpA "block_36"

markA "branch_35_dl"
cons 0
load
cons 3
sub
bEZA "branch_35_dr"
copyI 0
jumpA "block_36"

markA "branch_35_dr"
cons 0
load
cons 4
sub
bEZA "branch_35_ll"
lgt
jumpA "block_34"

markA "branch_35_ll"
cons 0
load
cons 5
sub
bEZA "branch_35_lr"
lgt
jumpA "block_34"

markA "branch_35_lr"
cons 0
load
cons 6
sub
bEZA "branch_35_ul"
discard
jumpA "block_18"

markA "branch_35_ul"
cons 0
load
cons 7
sub
bEZA "branch_35_ur"
discard
jumpA "block_18"

markA "branch_35_ur"

markA "block_36"
cons 0
load
cons 2
sub
bEZA "branch_36_dl"
mio outputchar
jumpA "block_37"

markA "branch_36_dl"
cons 0
load
cons 3
sub
bEZA "branch_36_dr"
mio outputchar
jumpA "block_37"

markA "branch_36_dr"
cons 0
load
cons 4
sub
bEZA "branch_36_ll"
mio inputchar
jumpA "block_34"

markA "branch_36_ll"
cons 0
load
cons 5
sub
bEZA "branch_36_lr"
mio inputchar
jumpA "block_34"

markA "branch_36_lr"
cons 0
load
cons 6
sub
bEZA "branch_36_ul"
div
jumpA "block_35"

markA "branch_36_ul"
cons 0
load
cons 7
sub
bEZA "branch_36_ur"
div
jumpA "block_35"

markA "branch_36_ur"

markA "block_37"
cons 0
load
cons 4
sub
bEZA "branch_37_ll"
cons 32
jumpA "block_34"

markA "branch_37_ll"
cons 0
load
cons 5
sub
bEZA "branch_37_lr"
cons 32
jumpA "block_34"

markA "branch_37_lr"
cons 0
load
cons 6
sub
bEZA "branch_37_ul"
cons 32
jumpA "block_34"

markA "branch_37_ul"
cons 0
load
cons 7
sub
bEZA "branch_37_ur"
sub
jumpA "block_36"

markA "branch_37_ur"

markA "block_38"
cons 0
load
cons 0
sub
bEZA "branch_38_rl"
sub
jumpA "block_33"

markA "branch_38_rl"
cons 0
load
cons 1
sub
bEZA "branch_38_rr"
sub
jumpA "block_33"

markA "branch_38_rr"
cons 0
load
cons 4
sub
bEZA "branch_38_ll"
mio outputchar
jumpA "block_28"

markA "branch_38_ll"
cons 0
load
cons 5
sub
bEZA "branch_38_lr"
mio outputchar
jumpA "block_28"

markA "branch_38_lr"
cons 0
load
cons 6
sub
bEZA "branch_38_ul"
mio outputchar
jumpA "block_28"

markA "branch_38_ul"
cons 0
load
cons 7
sub
bEZA "branch_38_ur"
mio outputchar
jumpA "block_28"

markA "branch_38_ur"
end
