cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 9
jumpA "block_1"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 9
jumpA "block_1"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
div
jumpA "block_8"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bEZA "branch_0_dr"
div
jumpA "block_8"

markA "branch_0_dr"

markA "block_1"
cons 0
load
cons 1
sub
bEZA "branch_1_rr"
cons 8
jumpA "block_5"

markA "branch_1_rr"
cons 0
load
cons 2
sub
bEZA "branch_1_dl"
lnot
jumpA "block_10"

markA "branch_1_dl"
cons 0
load
cons 3
sub
bEZA "branch_1_dr"
lnot
jumpA "block_8"

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

markA "block_3"
cons 0
load
cons 2
sub
bEZA "branch_3_dl"
cons 11
jumpA "block_16"

markA "branch_3_dl"
cons 0
load
cons 3
sub
bEZA "branch_3_dr"
cons 11
jumpA "block_16"

markA "branch_3_dr"
cons 0
load
cons 4
sub
bEZA "branch_3_ll"
mod
jumpA "block_1"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bEZA "branch_3_lr"
mod
jumpA "block_1"

markA "branch_3_lr"

markA "block_5"
cons 0
load
cons 0
sub
bEZA "branch_5_rl"
mul
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
mul
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 2
sub
bEZA "branch_5_dl"
mod
jumpA "block_10"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bEZA "branch_5_dr"
mod
jumpA "block_10"

markA "branch_5_dr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
discard
jumpA "block_1"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bEZA "branch_5_lr"
discard
jumpA "block_1"

markA "branch_5_lr"
cons 0
load
cons 6
sub
bEZA "branch_5_ul"
halibut
jumpA "block_3"

markA "branch_5_ul"
cons 0
load
cons 7
sub
bEZA "branch_5_ur"
halibut
jumpA "block_3"

markA "branch_5_ur"

markA "block_6"
cons 0
load
cons 0
sub
bEZA "branch_6_rl"
copyI 0
jumpA "block_7"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bEZA "branch_6_rr"
copyI 0
jumpA "block_7"

markA "branch_6_rr"
cons 0
load
cons 2
sub
bEZA "branch_6_dl"
mio inputdec
jumpA "block_11"

markA "branch_6_dl"
cons 0
load
cons 3
sub
bEZA "branch_6_dr"
mio inputdec
jumpA "block_11"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
mio outputdec
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
mio outputdec
jumpA "block_5"

markA "branch_6_lr"
cons 0
load
cons 6
sub
bEZA "branch_6_ul"
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
jumpA "block_3"

markA "branch_6_ul"
cons 0
load
cons 7
sub
bEZA "branch_6_ur"
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
jumpA "block_3"

markA "branch_6_ur"

markA "block_7"
cons 0
load
cons 0
sub
bEZA "branch_7_rl"
mio outputchar
jumpA "block_3"

markA "branch_7_rl"
cons 0
load
cons 1
sub
bEZA "branch_7_rr"
mio outputchar
jumpA "block_3"

markA "branch_7_rr"
cons 0
load
cons 2
sub
bEZA "branch_7_dl"
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
jumpA "block_12"

markA "branch_7_dl"
cons 0
load
cons 3
sub
bEZA "branch_7_dr"
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
jumpA "block_12"

markA "branch_7_dr"
cons 0
load
cons 4
sub
bEZA "branch_7_ll"
div
jumpA "block_6"

markA "branch_7_ll"
cons 0
load
cons 5
sub
bEZA "branch_7_lr"
div
jumpA "block_6"

markA "branch_7_lr"
cons 0
load
cons 6
sub
bEZA "branch_7_ul"
mio outputchar
jumpA "block_3"

markA "branch_7_ul"
cons 0
load
cons 7
sub
bEZA "branch_7_ur"
mio outputchar
jumpA "block_3"

markA "branch_7_ur"

markA "block_8"
cons 0
load
cons 1
sub
bEZA "branch_8_rr"
div
jumpA "block_13"

markA "branch_8_rr"
cons 0
load
cons 3
sub
bEZA "branch_8_dr"
add
jumpA "block_17"

markA "branch_8_dr"
cons 0
load
cons 6
sub
bEZA "branch_8_ul"
copyI 0
jumpA "block_0"

markA "branch_8_ul"
cons 0
load
cons 7
sub
bEZA "branch_8_ur"
halibut
jumpA "block_1"

markA "branch_8_ur"

markA "block_10"
cons 0
load
cons 0
sub
bEZA "branch_10_rl"
lgt
jumpA "block_11"

markA "branch_10_rl"
cons 0
load
cons 1
sub
bEZA "branch_10_rr"
lgt
jumpA "block_11"

markA "branch_10_rr"
cons 0
load
cons 2
sub
bEZA "branch_10_dl"
lgt
jumpA "block_11"

markA "branch_10_dl"
cons 0
load
cons 3
sub
bEZA "branch_10_dr"
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
jumpA "block_14"

markA "branch_10_dr"
cons 0
load
cons 6
sub
bEZA "branch_10_ul"
halibut
jumpA "block_1"

markA "branch_10_ul"
cons 0
load
cons 7
sub
bEZA "branch_10_ur"
mio inputdec
jumpA "block_5"

markA "branch_10_ur"

markA "block_11"
cons 0
load
cons 0
sub
bEZA "branch_11_rl"
mio inputchar
jumpA "block_3"

markA "branch_11_rl"
cons 0
load
cons 1
sub
bEZA "branch_11_rr"
sub
jumpA "block_23"

markA "branch_11_rr"
cons 0
load
cons 4
sub
bEZA "branch_11_ll"
cons 10
jumpA "block_14"

markA "branch_11_ll"
cons 0
load
cons 5
sub
bEZA "branch_11_lr"
cons 10
jumpA "block_14"

markA "branch_11_lr"
cons 0
load
cons 6
sub
bEZA "branch_11_ul"
mod
jumpA "block_6"

markA "branch_11_ul"
cons 0
load
cons 7
sub
bEZA "branch_11_ur"
mod
jumpA "block_6"

markA "branch_11_ur"

markA "block_12"
cons 0
load
cons 0
sub
bEZA "branch_12_rl"
div
jumpA "block_3"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bEZA "branch_12_rr"
div
jumpA "block_3"

markA "branch_12_rr"
cons 0
load
cons 2
sub
bEZA "branch_12_dl"
lgt
jumpA "block_11"

markA "branch_12_dl"
cons 0
load
cons 3
sub
bEZA "branch_12_dr"
lgt
jumpA "block_11"

markA "branch_12_dr"
cons 0
load
cons 4
sub
bEZA "branch_12_ll"
lgt
jumpA "block_11"

markA "branch_12_ll"
cons 0
load
cons 5
sub
bEZA "branch_12_lr"
lgt
jumpA "block_11"

markA "branch_12_lr"
cons 0
load
cons 6
sub
bEZA "branch_12_ul"
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
jumpA "block_7"

markA "branch_12_ul"
cons 0
load
cons 7
sub
bEZA "branch_12_ur"
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
jumpA "block_7"

markA "branch_12_ur"

markA "block_13"

markA "block_14"
cons 0
load
cons 0
sub
bEZA "branch_14_rl"
discard
jumpA "block_11"

markA "branch_14_rl"
cons 0
load
cons 1
sub
bEZA "branch_14_rr"
discard
jumpA "block_11"

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
jumpA "block_15"

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
jumpA "block_15"

markA "branch_14_dr"
cons 0
load
cons 4
sub
bEZA "branch_14_ll"
mio outputchar
jumpA "block_13"

markA "branch_14_ll"
cons 0
load
cons 5
sub
bEZA "branch_14_lr"
mio outputchar
jumpA "block_13"

markA "branch_14_lr"
cons 0
load
cons 6
sub
bEZA "branch_14_ul"
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

markA "branch_14_ul"
cons 0
load
cons 7
sub
bEZA "branch_14_ur"
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

markA "branch_14_ur"

markA "block_15"
cons 0
load
cons 0
sub
bEZA "branch_15_rl"
lgt
jumpA "block_11"

markA "branch_15_rl"
cons 0
load
cons 1
sub
bEZA "branch_15_rr"
lgt
jumpA "block_11"

markA "branch_15_rr"
cons 0
load
cons 4
sub
bEZA "branch_15_ll"
div
jumpA "block_13"

markA "branch_15_ll"
cons 0
load
cons 5
sub
bEZA "branch_15_lr"
div
jumpA "block_13"

markA "branch_15_lr"
cons 0
load
cons 6
sub
bEZA "branch_15_ul"
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
jumpA "block_14"

markA "branch_15_ul"
cons 0
load
cons 7
sub
bEZA "branch_15_ur"
lgt
jumpA "block_11"

markA "branch_15_ur"

markA "block_16"
cons 0
load
cons 2
sub
bEZA "branch_16_dl"
cons 3
jumpA "block_19"

markA "branch_16_dl"
cons 0
load
cons 3
sub
bEZA "branch_16_dr"
mul
jumpA "block_11"

markA "branch_16_dr"
cons 0
load
cons 4
sub
bEZA "branch_16_ll"
mul
jumpA "block_11"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bEZA "branch_16_lr"
mul
jumpA "block_11"

markA "branch_16_lr"
cons 0
load
cons 6
sub
bEZA "branch_16_ul"
mul
jumpA "block_11"

markA "branch_16_ul"
cons 0
load
cons 7
sub
bEZA "branch_16_ur"
mul
jumpA "block_11"

markA "branch_16_ur"

markA "block_17"
cons 0
load
cons 6
sub
bEZA "branch_17_ul"
mio inputchar
jumpA "block_8"

markA "branch_17_ul"
cons 0
load
cons 7
sub
bEZA "branch_17_ur"
mio inputchar
jumpA "block_8"

markA "branch_17_ur"

markA "block_19"
cons 0
load
cons 2
sub
bEZA "branch_19_dl"
mul
jumpA "block_20"

markA "branch_19_dl"
cons 0
load
cons 3
sub
bEZA "branch_19_dr"
mul
jumpA "block_20"

markA "branch_19_dr"
cons 0
load
cons 4
sub
bEZA "branch_19_ll"
sub
jumpA "block_11"

markA "branch_19_ll"
cons 0
load
cons 5
sub
bEZA "branch_19_lr"
sub
jumpA "block_11"

markA "branch_19_lr"
cons 0
load
cons 6
sub
bEZA "branch_19_ul"
discard
jumpA "block_16"

markA "branch_19_ul"
cons 0
load
cons 7
sub
bEZA "branch_19_ur"
discard
jumpA "block_16"

markA "branch_19_ur"

markA "block_20"
cons 0
load
cons 2
sub
bEZA "branch_20_dl"
add
jumpA "block_23"

markA "branch_20_dl"
cons 0
load
cons 3
sub
bEZA "branch_20_dr"
add
jumpA "block_23"

markA "branch_20_dr"
cons 0
load
cons 4
sub
bEZA "branch_20_ll"
discard
jumpA "block_11"

markA "branch_20_ll"
cons 0
load
cons 5
sub
bEZA "branch_20_lr"
discard
jumpA "block_11"

markA "branch_20_lr"
cons 0
load
cons 6
sub
bEZA "branch_20_ul"
mio outputdec
jumpA "block_19"

markA "branch_20_ul"
cons 0
load
cons 7
sub
bEZA "branch_20_ur"
mio outputdec
jumpA "block_19"

markA "branch_20_ur"

markA "block_23"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
mio outputchar
jumpA "block_11"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
mio outputchar
jumpA "block_11"

markA "branch_23_lr"
cons 0
load
cons 6
sub
bEZA "branch_23_ul"
mio inputchar
jumpA "block_20"

markA "branch_23_ul"
cons 0
load
cons 7
sub
bEZA "branch_23_ur"
mio inputchar
jumpA "block_20"

markA "branch_23_ur"
end
