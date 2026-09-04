cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
mio inputdec
jumpA "block_3"

markA "branch_0_dl"

markA "block_3"
cons 0
load
cons 2
sub
bEZA "branch_3_dl"
mio inputdec
jumpA "block_5"

markA "branch_3_dl"
cons 0
load
cons 3
sub
bEZA "branch_3_dr"
mio inputdec
jumpA "block_5"

markA "branch_3_dr"
cons 0
load
cons 6
sub
bEZA "branch_3_ul"
mod
jumpA "block_0"

markA "branch_3_ul"
cons 0
load
cons 7
sub
bEZA "branch_3_ur"
mod
jumpA "block_0"

markA "branch_3_ur"

markA "block_4"
cons 0
load
cons 3
sub
bEZA "branch_4_dr"
cons 4
jumpA "block_11"

markA "branch_4_dr"

markA "block_5"
cons 0
load
cons 2
sub
bEZA "branch_5_dl"
copyI 0
jumpA "block_6"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bEZA "branch_5_dr"
copyI 0
jumpA "block_6"

markA "branch_5_dr"
cons 0
load
cons 6
sub
bEZA "branch_5_ul"
mod
jumpA "block_3"

markA "branch_5_ul"
cons 0
load
cons 7
sub
bEZA "branch_5_ur"
mod
jumpA "block_3"

markA "branch_5_ur"

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
cons 3
sub
bEZA "branch_6_dr"
cons 3
jumpA "block_4"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
cons 3
jumpA "block_4"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
cons 3
jumpA "block_4"

markA "branch_6_lr"

markA "block_8"
cons 0
load
cons 0
sub
bEZA "branch_8_rl"
div
jumpA "block_9"

markA "branch_8_rl"
cons 0
load
cons 1
sub
bEZA "branch_8_rr"
div
jumpA "block_9"

markA "branch_8_rr"
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
cons 4
sub
bEZA "branch_9_ll"
copyI 0
jumpA "block_8"

markA "branch_9_ll"
cons 0
load
cons 5
sub
bEZA "branch_9_lr"
copyI 0
jumpA "block_8"

markA "branch_9_lr"

markA "block_11"
cons 0
load
cons 2
sub
bEZA "branch_11_dl"
halibut
jumpA "block_14"

markA "branch_11_dl"
cons 0
load
cons 3
sub
bEZA "branch_11_dr"
halibut
jumpA "block_14"

markA "branch_11_dr"
cons 0
load
cons 6
sub
bEZA "branch_11_ul"
discard
jumpA "block_4"

markA "branch_11_ul"
cons 0
load
cons 7
sub
bEZA "branch_11_ur"
discard
jumpA "block_4"

markA "branch_11_ur"

markA "block_12"
cons 0
load
cons 0
sub
bEZA "branch_12_rl"
jumpA "block_9"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bEZA "branch_12_rr"
discard
jumpA "block_23"

markA "branch_12_rr"

markA "block_14"
cons 0
load
cons 2
sub
bEZA "branch_14_dl"
mod
jumpA "block_19"

markA "branch_14_dl"
cons 0
load
cons 6
sub
bEZA "branch_14_ul"
lnot
jumpA "block_11"

markA "branch_14_ul"

markA "block_15"
cons 0
load
cons 0
sub
bEZA "branch_15_rl"
lnot
jumpA "block_16"

markA "branch_15_rl"
cons 0
load
cons 4
sub
bEZA "branch_15_ll"
lgt
jumpA "block_24"

markA "branch_15_ll"

markA "block_16"
cons 0
load
cons 0
sub
bEZA "branch_16_rl"
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

markA "branch_16_rl"
cons 0
load
cons 1
sub
bEZA "branch_16_rr"
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

markA "branch_16_rr"
cons 0
load
cons 4
sub
bEZA "branch_16_ll"
halibut
jumpA "block_15"

markA "branch_16_ll"
cons 0
load
cons 5
sub
bEZA "branch_16_lr"
halibut
jumpA "block_15"

markA "branch_16_lr"

markA "block_19"
cons 0
load
cons 2
sub
bEZA "branch_19_dl"
copyI 0
jumpA "block_22"

markA "branch_19_dl"
cons 0
load
cons 3
sub
bEZA "branch_19_dr"
copyI 0
jumpA "block_22"

markA "branch_19_dr"
cons 0
load
cons 6
sub
bEZA "branch_19_ul"
mio inputdec
jumpA "block_14"

markA "branch_19_ul"
cons 0
load
cons 7
sub
bEZA "branch_19_ur"
mio inputdec
jumpA "block_14"

markA "branch_19_ur"

markA "block_21"

markA "block_22"
cons 0
load
cons 2
sub
bEZA "branch_22_dl"
cons 1
jumpA "block_24"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bEZA "branch_22_dr"
cons 1
jumpA "block_24"

markA "branch_22_dr"
cons 0
load
cons 6
sub
bEZA "branch_22_ul"
div
jumpA "block_19"

markA "branch_22_ul"
cons 0
load
cons 7
sub
bEZA "branch_22_ur"
div
jumpA "block_19"

markA "branch_22_ur"

markA "block_23"
cons 0
load
cons 0
sub
bEZA "branch_23_rl"
mio outputdec
jumpA "block_21"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bEZA "branch_23_rr"
mio outputdec
jumpA "block_21"

markA "branch_23_rr"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
cons 1
jumpA "block_12"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
cons 1
jumpA "block_12"

markA "branch_23_lr"

markA "block_24"
cons 0
load
cons 0
sub
bEZA "branch_24_rl"
lgt
jumpA "block_15"

markA "branch_24_rl"
cons 0
load
cons 1
sub
bEZA "branch_24_rr"
lgt
jumpA "block_15"

markA "branch_24_rr"
end
