cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 11
jumpA "block_1"

markA "branch_0_rl"

markA "block_1"
cons 0
load
cons 0
sub
bEZA "branch_1_rl"
cons 5
jumpA "block_2"

markA "branch_1_rl"
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
cons 2
jumpA "block_3"

markA "branch_2_rl"
cons 0
load
cons 4
sub
bEZA "branch_2_ll"
discard
jumpA "block_1"

markA "branch_2_ll"
cons 0
load
cons 5
sub
bEZA "branch_2_lr"
discard
jumpA "block_1"

markA "branch_2_lr"

markA "block_3"
cons 0
load
cons 0
sub
bEZA "branch_3_rl"
mul
jumpA "block_4"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bEZA "branch_3_rr"
mul
jumpA "block_4"

markA "branch_3_rr"
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
cons 0
sub
bEZA "branch_4_rl"
mul
jumpA "block_5"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bEZA "branch_4_rr"
mul
jumpA "block_5"

markA "branch_4_rr"
cons 0
load
cons 4
sub
bEZA "branch_4_ll"
mio outputdec
jumpA "block_3"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bEZA "branch_4_lr"
mio outputdec
jumpA "block_3"

markA "branch_4_lr"

markA "block_5"
cons 0
load
cons 0
sub
bEZA "branch_5_rl"
copyI 0
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
copyI 0
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
mio outputdec
jumpA "block_4"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bEZA "branch_5_lr"
mio outputdec
jumpA "block_4"

markA "branch_5_lr"

markA "block_6"
cons 0
load
cons 0
sub
bEZA "branch_6_rl"
mio outputchar
jumpA "block_7"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bEZA "branch_6_rr"
mio outputchar
jumpA "block_7"

markA "branch_6_rr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
div
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
div
jumpA "block_5"

markA "branch_6_lr"

markA "block_7"
cons 0
load
cons 0
sub
bEZA "branch_7_rl"
mio inputdec
jumpA "block_8"

markA "branch_7_rl"
cons 0
load
cons 1
sub
bEZA "branch_7_rr"
mio inputdec
jumpA "block_8"

markA "branch_7_rr"
cons 0
load
cons 4
sub
bEZA "branch_7_ll"
sub
jumpA "block_6"

markA "branch_7_ll"
cons 0
load
cons 5
sub
bEZA "branch_7_lr"
sub
jumpA "block_6"

markA "branch_7_lr"

markA "block_8"
cons 0
load
cons 0
sub
bEZA "branch_8_rl"
cons 2
jumpA "block_9"

markA "branch_8_rl"
cons 0
load
cons 5
sub
bEZA "branch_8_lr"
mod
jumpA "block_7"

markA "branch_8_lr"

markA "block_9"
cons 0
load
cons 0
sub
bEZA "branch_9_rl"
cons 1
jumpA "block_10"

markA "branch_9_rl"
cons 0
load
cons 1
sub
bEZA "branch_9_rr"
cons 1
jumpA "block_10"

markA "branch_9_rr"
cons 0
load
cons 4
sub
bEZA "branch_9_ll"
discard
jumpA "block_8"

markA "branch_9_ll"
cons 0
load
cons 5
sub
bEZA "branch_9_lr"
discard
jumpA "block_8"

markA "branch_9_lr"

markA "block_10"
cons 0
load
cons 0
sub
bEZA "branch_10_rl"
halibut
jumpA "block_11"

markA "branch_10_rl"
cons 0
load
cons 1
sub
bEZA "branch_10_rr"
halibut
jumpA "block_11"

markA "branch_10_rr"
cons 0
load
cons 4
sub
bEZA "branch_10_ll"
discard
jumpA "block_9"

markA "branch_10_ll"
cons 0
load
cons 5
sub
bEZA "branch_10_lr"
discard
jumpA "block_9"

markA "branch_10_lr"

markA "block_11"
cons 0
load
cons 0
sub
bEZA "branch_11_rl"
mio outputchar
jumpA "block_12"

markA "branch_11_rl"
cons 0
load
cons 1
sub
bEZA "branch_11_rr"
mio outputchar
jumpA "block_12"

markA "branch_11_rr"
cons 0
load
cons 4
sub
bEZA "branch_11_ll"
lnot
jumpA "block_10"

markA "branch_11_ll"
cons 0
load
cons 5
sub
bEZA "branch_11_lr"
lnot
jumpA "block_10"

markA "branch_11_lr"

markA "block_12"
cons 0
load
cons 0
sub
bEZA "branch_12_rl"
mio inputdec
jumpA "block_13"

markA "branch_12_rl"
cons 0
load
cons 1
sub
bEZA "branch_12_rr"
mio inputdec
jumpA "block_13"

markA "branch_12_rr"
cons 0
load
cons 4
sub
bEZA "branch_12_ll"
sub
jumpA "block_11"

markA "branch_12_ll"
cons 0
load
cons 5
sub
bEZA "branch_12_lr"
sub
jumpA "block_11"

markA "branch_12_lr"

markA "block_13"
cons 0
load
cons 0
sub
bEZA "branch_13_rl"
copyI 0
jumpA "block_14"

markA "branch_13_rl"
cons 0
load
cons 1
sub
bEZA "branch_13_rr"
copyI 0
jumpA "block_14"

markA "branch_13_rr"
cons 0
load
cons 4
sub
bEZA "branch_13_ll"
mod
jumpA "block_12"

markA "branch_13_ll"
cons 0
load
cons 5
sub
bEZA "branch_13_lr"
mod
jumpA "block_12"

markA "branch_13_lr"

markA "block_14"
cons 0
load
cons 0
sub
bEZA "branch_14_rl"
cons 3
jumpA "block_15"

markA "branch_14_rl"
cons 0
load
cons 5
sub
bEZA "branch_14_lr"
div
jumpA "block_13"

markA "branch_14_lr"

markA "block_15"
cons 0
load
cons 0
sub
bEZA "branch_15_rl"
cons 2
jumpA "block_16"

markA "branch_15_rl"
cons 0
load
cons 4
sub
bEZA "branch_15_ll"
discard
jumpA "block_14"

markA "branch_15_ll"
cons 0
load
cons 5
sub
bEZA "branch_15_lr"
discard
jumpA "block_14"

markA "branch_15_lr"

markA "block_16"
cons 0
load
cons 0
sub
bEZA "branch_16_rl"
halibut
jumpA "block_17"

markA "branch_16_rl"
cons 0
load
cons 1
sub
bEZA "branch_16_rr"
halibut
jumpA "block_17"

markA "branch_16_rr"
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
copyI 0
jumpA "block_18"

markA "branch_17_rl"
cons 0
load
cons 1
sub
bEZA "branch_17_rr"
copyI 0
jumpA "block_18"

markA "branch_17_rr"
cons 0
load
cons 4
sub
bEZA "branch_17_ll"
lnot
jumpA "block_16"

markA "branch_17_ll"
cons 0
load
cons 5
sub
bEZA "branch_17_lr"
lnot
jumpA "block_16"

markA "branch_17_lr"

markA "block_18"
cons 0
load
cons 0
sub
bEZA "branch_18_rl"
cons 3
jumpA "block_19"

markA "branch_18_rl"
cons 0
load
cons 5
sub
bEZA "branch_18_lr"
div
jumpA "block_17"

markA "branch_18_lr"

markA "block_19"
cons 0
load
cons 0
sub
bEZA "branch_19_rl"
cons 1
jumpA "block_20"

markA "branch_19_rl"
cons 0
load
cons 1
sub
bEZA "branch_19_rr"
cons 1
jumpA "block_20"

markA "branch_19_rr"
cons 0
load
cons 4
sub
bEZA "branch_19_ll"
discard
jumpA "block_18"

markA "branch_19_ll"
cons 0
load
cons 5
sub
bEZA "branch_19_lr"
discard
jumpA "block_18"

markA "branch_19_lr"

markA "block_20"
cons 0
load
cons 0
sub
bEZA "branch_20_rl"
halibut
jumpA "block_21"

markA "branch_20_rl"
cons 0
load
cons 1
sub
bEZA "branch_20_rr"
halibut
jumpA "block_21"

markA "branch_20_rr"
cons 0
load
cons 4
sub
bEZA "branch_20_ll"
discard
jumpA "block_19"

markA "branch_20_ll"
cons 0
load
cons 5
sub
bEZA "branch_20_lr"
discard
jumpA "block_19"

markA "branch_20_lr"

markA "block_21"
cons 0
load
cons 0
sub
bEZA "branch_21_rl"
mio outputdec
jumpA "block_22"

markA "branch_21_rl"
cons 0
load
cons 1
sub
bEZA "branch_21_rr"
mio outputdec
jumpA "block_22"

markA "branch_21_rr"
cons 0
load
cons 4
sub
bEZA "branch_21_ll"
lnot
jumpA "block_20"

markA "branch_21_ll"
cons 0
load
cons 5
sub
bEZA "branch_21_lr"
lnot
jumpA "block_20"

markA "branch_21_lr"

markA "block_22"
cons 0
load
cons 0
sub
bEZA "branch_22_rl"
cons 43
jumpA "block_23"

markA "branch_22_rl"
cons 0
load
cons 2
sub
bEZA "branch_22_dl"
mio inputchar
jumpA "block_40"

markA "branch_22_dl"
cons 0
load
cons 3
sub
bEZA "branch_22_dr"
mio inputchar
jumpA "block_40"

markA "branch_22_dr"
cons 0
load
cons 4
sub
bEZA "branch_22_ll"
mio inputchar
jumpA "block_40"

markA "branch_22_ll"
cons 0
load
cons 5
sub
bEZA "branch_22_lr"
mul
jumpA "block_21"

markA "branch_22_lr"

markA "block_23"
cons 0
load
cons 0
sub
bEZA "branch_23_rl"
mio outputchar
jumpA "block_24"

markA "branch_23_rl"
cons 0
load
cons 1
sub
bEZA "branch_23_rr"
mio outputchar
jumpA "block_24"

markA "branch_23_rr"
cons 0
load
cons 4
sub
bEZA "branch_23_ll"
discard
jumpA "block_22"

markA "branch_23_ll"
cons 0
load
cons 5
sub
bEZA "branch_23_lr"
discard
jumpA "block_22"

markA "branch_23_lr"

markA "block_24"
cons 0
load
cons 0
sub
bEZA "branch_24_rl"
mio outputdec
jumpA "block_25"

markA "branch_24_rl"
cons 0
load
cons 1
sub
bEZA "branch_24_rr"
mio outputdec
jumpA "block_25"

markA "branch_24_rr"
cons 0
load
cons 4
sub
bEZA "branch_24_ll"
sub
jumpA "block_23"

markA "branch_24_ll"
cons 0
load
cons 5
sub
bEZA "branch_24_lr"
sub
jumpA "block_23"

markA "branch_24_lr"

markA "block_25"
cons 0
load
cons 0
sub
bEZA "branch_25_rl"
cons 61
jumpA "block_26"

markA "branch_25_rl"

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

markA "block_27"
cons 0
load
cons 0
sub
bEZA "branch_27_rl"
add
jumpA "block_28"

markA "branch_27_rl"
cons 0
load
cons 1
sub
bEZA "branch_27_rr"
add
jumpA "block_28"

markA "branch_27_rr"
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

markA "block_28"
cons 0
load
cons 0
sub
bEZA "branch_28_rl"
mio outputdec
jumpA "block_29"

markA "branch_28_rl"
cons 0
load
cons 1
sub
bEZA "branch_28_rr"
mio outputdec
jumpA "block_29"

markA "branch_28_rr"
cons 0
load
cons 4
sub
bEZA "branch_28_ll"
mio inputchar
jumpA "block_27"

markA "branch_28_ll"
cons 0
load
cons 5
sub
bEZA "branch_28_lr"
mio inputchar
jumpA "block_27"

markA "branch_28_lr"

markA "block_29"
cons 0
load
cons 0
sub
bEZA "branch_29_rl"
cons 3
jumpA "block_30"

markA "branch_29_rl"
cons 0
load
cons 5
sub
bEZA "branch_29_lr"
mul
jumpA "block_28"

markA "branch_29_lr"

markA "block_30"
cons 0
load
cons 2
sub
bEZA "branch_30_dl"
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
jumpA "block_46"

markA "branch_30_dl"
cons 0
load
cons 3
sub
bEZA "branch_30_dr"
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
jumpA "block_46"

markA "branch_30_dr"
cons 0
load
cons 4
sub
bEZA "branch_30_ll"
discard
jumpA "block_29"

markA "branch_30_ll"
cons 0
load
cons 5
sub
bEZA "branch_30_lr"
discard
jumpA "block_29"

markA "branch_30_lr"

markA "block_36"
cons 0
load
cons 4
sub
bEZA "branch_36_ll"
mio inputdec
jumpA "block_38"

markA "branch_36_ll"
cons 0
load
cons 5
sub
bEZA "branch_36_lr"
mio inputdec
jumpA "block_38"

markA "branch_36_lr"

markA "block_37"
cons 0
load
cons 0
sub
bEZA "branch_37_rl"
mio inputdec
jumpA "block_40"

markA "branch_37_rl"
cons 0
load
cons 1
sub
bEZA "branch_37_rr"
mio inputdec
jumpA "block_40"

markA "branch_37_rr"
cons 0
load
cons 4
sub
bEZA "branch_37_ll"
mio inputdec
jumpA "block_44"

markA "branch_37_ll"
cons 0
load
cons 5
sub
bEZA "branch_37_lr"
mio inputdec
jumpA "block_44"

markA "branch_37_lr"

markA "block_38"
cons 0
load
cons 0
sub
bEZA "branch_38_rl"
mod
jumpA "block_36"

markA "branch_38_rl"
cons 0
load
cons 1
sub
bEZA "branch_38_rr"
mod
jumpA "block_36"

markA "branch_38_rr"

markA "block_39"
cons 0
load
cons 0
sub
bEZA "branch_39_rl"
lnot
jumpA "block_44"

markA "branch_39_rl"
cons 0
load
cons 1
sub
bEZA "branch_39_rr"
lnot
jumpA "block_44"

markA "branch_39_rr"
cons 0
load
cons 4
sub
bEZA "branch_39_ll"
lnot
jumpA "block_43"

markA "branch_39_ll"
cons 0
load
cons 5
sub
bEZA "branch_39_lr"
lnot
jumpA "block_43"

markA "branch_39_lr"

markA "block_40"
cons 0
load
cons 1
sub
bEZA "branch_40_rr"
cons 25
jumpA "block_45"

markA "branch_40_rr"
cons 0
load
cons 4
sub
bEZA "branch_40_ll"
mod
jumpA "block_37"

markA "branch_40_ll"
cons 0
load
cons 5
sub
bEZA "branch_40_lr"
mod
jumpA "block_37"

markA "branch_40_lr"
cons 0
load
cons 7
sub
bEZA "branch_40_ur"
add
jumpA "block_22"

markA "branch_40_ur"

markA "block_41"
cons 0
load
cons 0
sub
bEZA "branch_41_rl"
cons 13
jumpA "block_42"

markA "branch_41_rl"
cons 0
load
cons 1
sub
bEZA "branch_41_rr"
cons 13
jumpA "block_42"

markA "branch_41_rr"
cons 0
load
cons 4
sub
bEZA "branch_41_ll"
mod
jumpA "block_36"

markA "branch_41_ll"
cons 0
load
cons 5
sub
bEZA "branch_41_lr"
mod
jumpA "block_36"

markA "branch_41_lr"
cons 0
load
cons 6
sub
bEZA "branch_41_ul"
mod
jumpA "block_36"

markA "branch_41_ul"
cons 0
load
cons 7
sub
bEZA "branch_41_ur"
mod
jumpA "block_36"

markA "branch_41_ur"

markA "block_42"
cons 0
load
cons 0
sub
bEZA "branch_42_rl"
discard
jumpA "block_43"

markA "branch_42_rl"
cons 0
load
cons 1
sub
bEZA "branch_42_rr"
discard
jumpA "block_43"

markA "branch_42_rr"
cons 0
load
cons 4
sub
bEZA "branch_42_ll"
discard
jumpA "block_41"

markA "branch_42_ll"
cons 0
load
cons 5
sub
bEZA "branch_42_lr"
discard
jumpA "block_41"

markA "branch_42_lr"

markA "block_43"
cons 0
load
cons 0
sub
bEZA "branch_43_rl"
halibut
jumpA "block_39"

markA "branch_43_rl"
cons 0
load
cons 1
sub
bEZA "branch_43_rr"
halibut
jumpA "block_39"

markA "branch_43_rr"
cons 0
load
cons 4
sub
bEZA "branch_43_ll"
cons 9
jumpA "block_42"

markA "branch_43_ll"
cons 0
load
cons 5
sub
bEZA "branch_43_lr"
cons 9
jumpA "block_42"

markA "branch_43_lr"

markA "block_44"
cons 0
load
cons 0
sub
bEZA "branch_44_rl"
mod
jumpA "block_37"

markA "branch_44_rl"
cons 0
load
cons 1
sub
bEZA "branch_44_rr"
mod
jumpA "block_37"

markA "branch_44_rr"
cons 0
load
cons 4
sub
bEZA "branch_44_ll"
halibut
jumpA "block_39"

markA "branch_44_ll"
cons 0
load
cons 5
sub
bEZA "branch_44_lr"
halibut
jumpA "block_39"

markA "branch_44_lr"

markA "block_45"
cons 0
load
cons 4
sub
bEZA "branch_45_ll"
discard
jumpA "block_40"

markA "branch_45_ll"
cons 0
load
cons 5
sub
bEZA "branch_45_lr"
discard
jumpA "block_40"

markA "branch_45_lr"

markA "block_46"
end
