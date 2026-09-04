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

markA "block_1"
cons 0
load
cons 0
sub
bEZA "branch_1_rl"
mio inputdec
jumpA "block_2"

markA "branch_1_rl"
cons 0
load
cons 1
sub
bEZA "branch_1_rr"
mio inputdec
jumpA "block_2"

markA "branch_1_rr"
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
copyI 0
jumpA "block_3"

markA "branch_2_rl"
cons 0
load
cons 1
sub
bEZA "branch_2_rr"
copyI 0
jumpA "block_3"

markA "branch_2_rr"
cons 0
load
cons 2
sub
bEZA "branch_2_dl"
jumpA "block_33"

markA "branch_2_dl"
cons 0
load
cons 3
sub
bEZA "branch_2_dr"
jumpA "block_33"

markA "branch_2_dr"
cons 0
load
cons 4
sub
bEZA "branch_2_ll"
mod
jumpA "block_1"

markA "branch_2_ll"
cons 0
load
cons 5
sub
bEZA "branch_2_lr"
mod
jumpA "block_1"

markA "branch_2_lr"

markA "block_3"
cons 0
load
cons 0
sub
bEZA "branch_3_rl"
cons 1
jumpA "block_4"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bEZA "branch_3_rr"
cons 1
jumpA "block_4"

markA "branch_3_rr"
cons 0
load
cons 4
sub
bEZA "branch_3_ll"
div
jumpA "block_2"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bEZA "branch_3_lr"
div
jumpA "block_2"

markA "branch_3_lr"

markA "block_4"
cons 0
load
cons 0
sub
bEZA "branch_4_rl"
lgt
jumpA "block_5"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bEZA "branch_4_rr"
lgt
jumpA "block_5"

markA "branch_4_rr"
cons 0
load
cons 4
sub
bEZA "branch_4_ll"
discard
jumpA "block_3"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bEZA "branch_4_lr"
discard
jumpA "block_3"

markA "branch_4_lr"

markA "block_5"
cons 0
load
cons 0
sub
bEZA "branch_5_rl"
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
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
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
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
lgt
jumpA "block_4"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bEZA "branch_5_lr"
lgt
jumpA "block_4"

markA "branch_5_lr"

markA "block_6"
cons 0
load
cons 0
sub
bEZA "branch_6_rl"
jumpA "block_8"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bEZA "branch_6_rr"
jumpA "block_8"

markA "branch_6_rr"
cons 0
load
cons 2
sub
bEZA "branch_6_dl"
jumpA "block_19"

markA "branch_6_dl"
cons 0
load
cons 3
sub
bEZA "branch_6_dr"
jumpA "block_19"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
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
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
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
jumpA "block_5"

markA "branch_6_lr"

markA "block_8"
cons 0
load
cons 0
sub
bEZA "branch_8_rl"
discard
jumpA "block_9"

markA "branch_8_rl"
cons 0
load
cons 1
sub
bEZA "branch_8_rr"
discard
jumpA "block_9"

markA "branch_8_rr"
cons 0
load
cons 2
sub
bEZA "branch_8_dl"
cons 1
jumpA "block_17"

markA "branch_8_dl"
cons 0
load
cons 3
sub
bEZA "branch_8_dr"
cons 1
jumpA "block_17"

markA "branch_8_dr"
cons 0
load
cons 4
sub
bEZA "branch_8_ll"
jumpA "block_6"

markA "branch_8_ll"
cons 0
load
cons 5
sub
bEZA "branch_8_lr"
jumpA "block_6"

markA "branch_8_lr"

markA "block_9"
cons 0
load
cons 0
sub
bEZA "branch_9_rl"
mio outputdec
jumpA "block_10"

markA "branch_9_rl"
cons 0
load
cons 1
sub
bEZA "branch_9_rr"
mio outputdec
jumpA "block_10"

markA "branch_9_rr"
cons 0
load
cons 2
sub
bEZA "branch_9_dl"
discard
jumpA "block_17"

markA "branch_9_dl"
cons 0
load
cons 3
sub
bEZA "branch_9_dr"
discard
jumpA "block_17"

markA "branch_9_dr"
cons 0
load
cons 4
sub
bEZA "branch_9_ll"
cons 1
jumpA "block_8"

markA "branch_9_ll"
cons 0
load
cons 5
sub
bEZA "branch_9_lr"
cons 1
jumpA "block_8"

markA "branch_9_lr"

markA "block_10"
cons 0
load
cons 0
sub
bEZA "branch_10_rl"
jumpA "block_12"

markA "branch_10_rl"
cons 0
load
cons 1
sub
bEZA "branch_10_rr"
jumpA "block_12"

markA "branch_10_rr"
cons 0
load
cons 2
sub
bEZA "branch_10_dl"
sub
jumpA "block_17"

markA "branch_10_dl"
cons 0
load
cons 3
sub
bEZA "branch_10_dr"
sub
jumpA "block_17"

markA "branch_10_dr"
cons 0
load
cons 4
sub
bEZA "branch_10_ll"
mul
jumpA "block_9"

markA "branch_10_ll"
cons 0
load
cons 5
sub
bEZA "branch_10_lr"
mul
jumpA "block_9"

markA "branch_10_lr"

markA "block_12"

markA "block_17"
cons 0
load
cons 0
sub
bEZA "branch_17_rl"
mod
jumpA "block_12"

markA "branch_17_rl"
cons 0
load
cons 1
sub
bEZA "branch_17_rr"
mod
jumpA "block_12"

markA "branch_17_rr"
cons 0
load
cons 2
sub
bEZA "branch_17_dl"
mod
jumpA "block_12"

markA "branch_17_dl"
cons 0
load
cons 4
sub
bEZA "branch_17_ll"
discard
jumpA "block_19"

markA "branch_17_ll"
cons 0
load
cons 5
sub
bEZA "branch_17_lr"
cons 0
cons 6
store
jumpA "block_6"

markA "branch_17_lr"
cons 0
load
cons 6
sub
bEZA "branch_17_ul"
cons 0
cons 1
store
jumpA "block_8"

markA "branch_17_ul"
cons 0
load
cons 7
sub
bEZA "branch_17_ur"
cons 0
cons 0
store
jumpA "block_12"

markA "branch_17_ur"

markA "block_19"
cons 0
load
cons 0
sub
bEZA "branch_19_rl"
cons 1
jumpA "block_17"

markA "branch_19_rl"
cons 0
load
cons 1
sub
bEZA "branch_19_rr"
cons 1
jumpA "block_17"

markA "branch_19_rr"
cons 0
load
cons 2
sub
bEZA "branch_19_dl"
copyI 0
jumpA "block_20"

markA "branch_19_dl"
cons 0
load
cons 3
sub
bEZA "branch_19_dr"
copyI 0
jumpA "block_20"

markA "branch_19_dr"
cons 0
load
cons 6
sub
bEZA "branch_19_ul"
jumpA "block_6"

markA "branch_19_ul"
cons 0
load
cons 7
sub
bEZA "branch_19_ur"
jumpA "block_6"

markA "branch_19_ur"

markA "block_20"
cons 0
load
cons 1
sub
bEZA "branch_20_rr"
mio inputdec
jumpA "block_24"

markA "branch_20_rr"
cons 0
load
cons 2
sub
bEZA "branch_20_dl"
cons 3
jumpA "block_26"

markA "branch_20_dl"
cons 0
load
cons 3
sub
bEZA "branch_20_dr"
cons 3
jumpA "block_26"

markA "branch_20_dr"
cons 0
load
cons 6
sub
bEZA "branch_20_ul"
div
jumpA "block_19"

markA "branch_20_ul"
cons 0
load
cons 7
sub
bEZA "branch_20_ur"
div
jumpA "block_19"

markA "branch_20_ur"

markA "block_24"
cons 0
load
cons 4
sub
bEZA "branch_24_ll"
mod
jumpA "block_20"

markA "branch_24_ll"
cons 0
load
cons 5
sub
bEZA "branch_24_lr"
mod
jumpA "block_20"

markA "branch_24_lr"

markA "block_26"
cons 0
load
cons 2
sub
bEZA "branch_26_dl"
cons 2
jumpA "block_28"

markA "branch_26_dl"
cons 0
load
cons 3
sub
bEZA "branch_26_dr"
cons 2
jumpA "block_28"

markA "branch_26_dr"
cons 0
load
cons 6
sub
bEZA "branch_26_ul"
discard
jumpA "block_20"

markA "branch_26_ul"
cons 0
load
cons 7
sub
bEZA "branch_26_ur"
discard
jumpA "block_20"

markA "branch_26_ur"

markA "block_28"
cons 0
load
cons 2
sub
bEZA "branch_28_dl"
halibut
jumpA "block_29"

markA "branch_28_dl"
cons 0
load
cons 3
sub
bEZA "branch_28_dr"
halibut
jumpA "block_29"

markA "branch_28_dr"
cons 0
load
cons 6
sub
bEZA "branch_28_ul"
discard
jumpA "block_26"

markA "branch_28_ul"
cons 0
load
cons 7
sub
bEZA "branch_28_ur"
discard
jumpA "block_26"

markA "branch_28_ur"

markA "block_29"
cons 0
load
cons 2
sub
bEZA "branch_29_dl"
mul
jumpA "block_30"

markA "branch_29_dl"
cons 0
load
cons 3
sub
bEZA "branch_29_dr"
mul
jumpA "block_30"

markA "branch_29_dr"
cons 0
load
cons 6
sub
bEZA "branch_29_ul"
lnot
jumpA "block_28"

markA "branch_29_ul"
cons 0
load
cons 7
sub
bEZA "branch_29_ur"
lnot
jumpA "block_28"

markA "branch_29_ur"

markA "block_30"
cons 0
load
cons 2
sub
bEZA "branch_30_dl"
cons 2
jumpA "block_31"

markA "branch_30_dl"
cons 0
load
cons 3
sub
bEZA "branch_30_dr"
cons 2
jumpA "block_31"

markA "branch_30_dr"
cons 0
load
cons 6
sub
bEZA "branch_30_ul"
mio outputdec
jumpA "block_29"

markA "branch_30_ul"
cons 0
load
cons 7
sub
bEZA "branch_30_ur"
mio outputdec
jumpA "block_29"

markA "branch_30_ur"

markA "block_31"
cons 0
load
cons 2
sub
bEZA "branch_31_dl"
cons 1
jumpA "block_32"

markA "branch_31_dl"
cons 0
load
cons 3
sub
bEZA "branch_31_dr"
cons 1
jumpA "block_32"

markA "branch_31_dr"
cons 0
load
cons 6
sub
bEZA "branch_31_ul"
discard
jumpA "block_30"

markA "branch_31_ul"
cons 0
load
cons 7
sub
bEZA "branch_31_ur"
discard
jumpA "block_30"

markA "branch_31_ur"

markA "block_32"
cons 0
load
cons 2
sub
bEZA "branch_32_dl"
halibut
jumpA "block_34"

markA "branch_32_dl"
cons 0
load
cons 3
sub
bEZA "branch_32_dr"
halibut
jumpA "block_34"

markA "branch_32_dr"
cons 0
load
cons 6
sub
bEZA "branch_32_ul"
discard
jumpA "block_31"

markA "branch_32_ul"
cons 0
load
cons 7
sub
bEZA "branch_32_ur"
discard
jumpA "block_31"

markA "branch_32_ur"

markA "block_33"
cons 0
load
cons 2
sub
bEZA "branch_33_dl"
mio outputchar
jumpA "block_35"

markA "branch_33_dl"
cons 0
load
cons 3
sub
bEZA "branch_33_dr"
mio outputchar
jumpA "block_35"

markA "branch_33_dr"
cons 0
load
cons 6
sub
bEZA "branch_33_ul"
jumpA "block_2"

markA "branch_33_ul"
cons 0
load
cons 7
sub
bEZA "branch_33_ur"
jumpA "block_2"

markA "branch_33_ur"

markA "block_34"
cons 0
load
cons 2
sub
bEZA "branch_34_dl"
cons 1
jumpA "block_36"

markA "branch_34_dl"
cons 0
load
cons 3
sub
bEZA "branch_34_dr"
cons 1
jumpA "block_36"

markA "branch_34_dr"
cons 0
load
cons 6
sub
bEZA "branch_34_ul"
lnot
jumpA "block_32"

markA "branch_34_ul"
cons 0
load
cons 7
sub
bEZA "branch_34_ur"
lnot
jumpA "block_32"

markA "branch_34_ur"

markA "block_35"
cons 0
load
cons 2
sub
bEZA "branch_35_dl"
discard
jumpA "block_37"

markA "branch_35_dl"
cons 0
load
cons 3
sub
bEZA "branch_35_dr"
discard
jumpA "block_37"

markA "branch_35_dr"
cons 0
load
cons 6
sub
bEZA "branch_35_ul"
sub
jumpA "block_33"

markA "branch_35_ul"
cons 0
load
cons 7
sub
bEZA "branch_35_ur"
sub
jumpA "block_33"

markA "branch_35_ur"

markA "block_36"
cons 0
load
cons 2
sub
bEZA "branch_36_dl"
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
jumpA "block_39"

markA "branch_36_dl"
cons 0
load
cons 3
sub
bEZA "branch_36_dr"
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
jumpA "block_39"

markA "branch_36_dr"
cons 0
load
cons 6
sub
bEZA "branch_36_ul"
discard
jumpA "block_34"

markA "branch_36_ul"
cons 0
load
cons 7
sub
bEZA "branch_36_ur"
discard
jumpA "block_34"

markA "branch_36_ur"

markA "block_37"
cons 0
load
cons 0
sub
bEZA "branch_37_rl"
jumpA "block_39"

markA "branch_37_rl"
cons 0
load
cons 1
sub
bEZA "branch_37_rr"
jumpA "block_39"

markA "branch_37_rr"
cons 0
load
cons 6
sub
bEZA "branch_37_ul"
cons 1
jumpA "block_35"

markA "branch_37_ul"
cons 0
load
cons 7
sub
bEZA "branch_37_ur"
cons 1
jumpA "block_35"

markA "branch_37_ur"

markA "block_39"
cons 0
load
cons 4
sub
bEZA "branch_39_ll"
jumpA "block_37"

markA "branch_39_ll"
cons 0
load
cons 5
sub
bEZA "branch_39_lr"
jumpA "block_37"

markA "branch_39_lr"
cons 0
load
cons 6
sub
bEZA "branch_39_ul"
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
jumpA "block_36"

markA "branch_39_ul"
cons 0
load
cons 7
sub
bEZA "branch_39_ur"
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
jumpA "block_36"

markA "branch_39_ur"
end
