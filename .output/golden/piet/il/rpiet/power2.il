storeID 0 0

markA "block_0"
loadD 0
bNEA "branch_0_rl"
inputDS
jumpA "block_3"

markA "branch_0_rl"
loadsubDI 0 1
bNEA "branch_0_rr"
storeID 0 0
inputDS
jumpA "block_3"

markA "branch_0_rr"
loadsubDI 0 2
bNEA "branch_0_dl"
inputCS
jumpA "block_37"

markA "branch_0_dl"
loadsubDI 0 3
bNEA "branch_0_dr"
inputCS
jumpA "block_37"

markA "branch_0_dr"
loadsubDI 0 4
bNEA "branch_0_ll"
storeID 0 0
inputDS
jumpA "block_3"

markA "branch_0_ll"
loadsubDI 0 5
bNEA "branch_0_lr"
storeID 0 0
inputDS
jumpA "block_3"

markA "branch_0_lr"
loadsubDI 0 6
bNEA "branch_0_ul"
storeID 0 0
inputDS
jumpA "block_3"

markA "branch_0_ul"
loadsubDI 0 7
bNEA "branch_0_ur"
storeID 0 0
inputDS
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_3"
loadD 0
bNEA "branch_3_rl"
inputDS
jumpA "block_4"

markA "branch_3_rl"
loadsubDI 0 1
bNEA "branch_3_rr"
inputDS
jumpA "block_4"

markA "branch_3_rr"
loadsubDI 0 2
bNEA "branch_3_dl"
storeID 5 0
mod
jumpA "block_0"

markA "branch_3_dl"
loadsubDI 0 3
bNEA "branch_3_dr"
storeID 4 0
mod
jumpA "block_0"

markA "branch_3_dr"
loadsubDI 0 4
bNEA "branch_3_ll"
mod
jumpA "block_0"

markA "branch_3_ll"
loadsubDI 0 5
bNEA "branch_3_lr"
mod
jumpA "block_0"

markA "branch_3_lr"
loadsubDI 0 6
bNEA "branch_3_ul"
storeID 1 0
inputDS
jumpA "block_4"

markA "branch_3_ul"
loadsubDI 0 7
bNEA "branch_3_ur"
storeID 0 0
inputDS
jumpA "block_4"

markA "branch_3_ur"
end

markA "block_4"
loadD 0
bNEA "branch_4_rl"
copy I 0
jumpA "block_5"

markA "branch_4_rl"
loadsubDI 0 1
bNEA "branch_4_rr"
copy I 0
jumpA "block_5"

markA "branch_4_rr"
loadsubDI 0 2
bNEA "branch_4_dl"
storeID 5 0
mod
jumpA "block_3"

markA "branch_4_dl"
loadsubDI 0 3
bNEA "branch_4_dr"
storeID 4 0
mod
jumpA "block_3"

markA "branch_4_dr"
loadsubDI 0 4
bNEA "branch_4_ll"
mod
jumpA "block_3"

markA "branch_4_ll"
loadsubDI 0 5
bNEA "branch_4_lr"
mod
jumpA "block_3"

markA "branch_4_lr"
loadsubDI 0 6
bNEA "branch_4_ul"
storeID 1 0
copy I 0
jumpA "block_5"

markA "branch_4_ul"
loadsubDI 0 7
bNEA "branch_4_ur"
storeID 0 0
copy I 0
jumpA "block_5"

markA "branch_4_ur"
end

markA "block_5"
loadD 0
bNEA "branch_5_rl"
cons 1
jumpA "block_6"

markA "branch_5_rl"
loadsubDI 0 1
bNEA "branch_5_rr"
cons 1
jumpA "block_6"

markA "branch_5_rr"
loadsubDI 0 2
bNEA "branch_5_dl"
storeID 6 0
jumpA "block_5"

markA "branch_5_dl"
loadsubDI 0 3
bNEA "branch_5_dr"
storeID 7 0
jumpA "block_5"

markA "branch_5_dr"
loadsubDI 0 4
bNEA "branch_5_ll"
div
jumpA "block_4"

markA "branch_5_ll"
loadsubDI 0 5
bNEA "branch_5_lr"
div
jumpA "block_4"

markA "branch_5_lr"
loadsubDI 0 6
bNEA "branch_5_ul"
storeID 1 0
cons 1
jumpA "block_6"

markA "branch_5_ul"
loadsubDI 0 7
bNEA "branch_5_ur"
storeID 0 0
cons 1
jumpA "block_6"

markA "branch_5_ur"
end

markA "block_6"
loadD 0
bNEA "branch_6_rl"
cons 1
jumpA "block_7"

markA "branch_6_rl"
loadsubDI 0 1
bNEA "branch_6_rr"
cons 1
jumpA "block_7"

markA "branch_6_rr"
loadsubDI 0 2
bNEA "branch_6_dl"
storeID 6 0
jumpA "block_5"

markA "branch_6_dl"
loadsubDI 0 3
bNEA "branch_6_dr"
storeID 7 0
jumpA "block_5"

markA "branch_6_dr"
loadsubDI 0 4
bNEA "branch_6_ll"
discard
jumpA "block_5"

markA "branch_6_ll"
loadsubDI 0 5
bNEA "branch_6_lr"
discard
jumpA "block_5"

markA "branch_6_lr"
loadsubDI 0 6
bNEA "branch_6_ul"
storeID 1 0
cons 1
jumpA "block_7"

markA "branch_6_ul"
loadsubDI 0 7
bNEA "branch_6_ur"
storeID 0 0
cons 1
jumpA "block_7"

markA "branch_6_ur"
end

markA "block_7"
loadD 0
bNEA "branch_7_rl"
sub
jumpA "block_8"

markA "branch_7_rl"
loadsubDI 0 1
bNEA "branch_7_rr"
sub
jumpA "block_8"

markA "branch_7_rr"
loadsubDI 0 2
bNEA "branch_7_dl"
storeID 6 0
jumpA "block_5"

markA "branch_7_dl"
loadsubDI 0 3
bNEA "branch_7_dr"
storeID 7 0
jumpA "block_5"

markA "branch_7_dr"
loadsubDI 0 4
bNEA "branch_7_ll"
discard
jumpA "block_6"

markA "branch_7_ll"
loadsubDI 0 5
bNEA "branch_7_lr"
discard
jumpA "block_6"

markA "branch_7_lr"
loadsubDI 0 6
bNEA "branch_7_ul"
storeID 1 0
sub
jumpA "block_8"

markA "branch_7_ul"
loadsubDI 0 7
bNEA "branch_7_ur"
storeID 0 0
sub
jumpA "block_8"

markA "branch_7_ur"
end

markA "block_8"
loadD 0
bNEA "branch_8_rl"
lgt
jumpA "block_9"

markA "branch_8_rl"
loadsubDI 0 1
bNEA "branch_8_rr"
lgt
jumpA "block_9"

markA "branch_8_rr"
loadsubDI 0 2
bNEA "branch_8_dl"
storeID 6 0
jumpA "block_8"

markA "branch_8_dl"
loadsubDI 0 3
bNEA "branch_8_dr"
storeID 7 0
jumpA "block_8"

markA "branch_8_dr"
loadsubDI 0 4
bNEA "branch_8_ll"
outputCMaybeS
jumpA "block_7"

markA "branch_8_ll"
loadsubDI 0 5
bNEA "branch_8_lr"
outputCMaybeS
jumpA "block_7"

markA "branch_8_lr"
loadsubDI 0 6
bNEA "branch_8_ul"
storeID 1 0
lgt
jumpA "block_9"

markA "branch_8_ul"
loadsubDI 0 7
bNEA "branch_8_ur"
storeID 0 0
lgt
jumpA "block_9"

markA "branch_8_ur"
end

markA "block_9"
loadD 0
bNEA "branch_9_rl"
lnot
jumpA "block_10"

markA "branch_9_rl"
loadsubDI 0 1
bNEA "branch_9_rr"
lnot
jumpA "block_10"

markA "branch_9_rr"
loadsubDI 0 2
bNEA "branch_9_dl"
storeID 6 0
jumpA "block_8"

markA "branch_9_dl"
loadsubDI 0 3
bNEA "branch_9_dr"
storeID 7 0
jumpA "block_8"

markA "branch_9_dr"
loadsubDI 0 4
bNEA "branch_9_ll"
lgt
jumpA "block_8"

markA "branch_9_ll"
loadsubDI 0 5
bNEA "branch_9_lr"
lgt
jumpA "block_8"

markA "branch_9_lr"
loadsubDI 0 6
bNEA "branch_9_ul"
storeID 1 0
lnot
jumpA "block_10"

markA "branch_9_ul"
loadsubDI 0 7
bNEA "branch_9_ur"
storeID 0 0
lnot
jumpA "block_10"

markA "branch_9_ur"
end

markA "block_10"
loadD 0
bNEA "branch_10_rl"
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_11"

markA "branch_10_rl"
loadsubDI 0 1
bNEA "branch_10_rr"
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_11"

markA "branch_10_rr"
loadsubDI 0 2
bNEA "branch_10_dl"
storeID 6 0
jumpA "block_8"

markA "branch_10_dl"
loadsubDI 0 3
bNEA "branch_10_dr"
storeID 7 0
jumpA "block_8"

markA "branch_10_dr"
loadsubDI 0 4
bNEA "branch_10_ll"
roll
jumpA "block_9"

markA "branch_10_ll"
loadsubDI 0 5
bNEA "branch_10_lr"
roll
jumpA "block_9"

markA "branch_10_lr"
loadsubDI 0 6
bNEA "branch_10_ul"
storeID 1 0
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_11"

markA "branch_10_ul"
loadsubDI 0 7
bNEA "branch_10_ur"
storeID 0 0
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_11"

markA "branch_10_ur"
end

markA "block_11"
loadD 0
bNEA "branch_11_rl"
cons 2
jumpA "block_12"

markA "branch_11_rl"
loadsubDI 0 1
bNEA "branch_11_rr"
cons 2
jumpA "block_12"

markA "branch_11_rr"
loadsubDI 0 2
bNEA "branch_11_dl"
discard
jumpA "block_31"

markA "branch_11_dl"
loadsubDI 0 3
bNEA "branch_11_dr"
storeID 7 0
jumpA "block_8"

markA "branch_11_dr"
loadsubDI 0 4
bNEA "branch_11_ll"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_10"

markA "branch_11_ll"
loadsubDI 0 5
bNEA "branch_11_lr"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_10"

markA "branch_11_lr"
loadsubDI 0 6
bNEA "branch_11_ul"
storeID 1 0
cons 2
jumpA "block_12"

markA "branch_11_ul"
loadsubDI 0 7
bNEA "branch_11_ur"
storeID 0 0
cons 2
jumpA "block_12"

markA "branch_11_ur"
end

markA "block_12"
loadD 0
bNEA "branch_12_rl"
cons 1
jumpA "block_13"

markA "branch_12_rl"
loadsubDI 0 1
bNEA "branch_12_rr"
cons 1
jumpA "block_13"

markA "branch_12_rr"
loadsubDI 0 2
bNEA "branch_12_dl"
storeID 6 0
jumpA "block_8"

markA "branch_12_dl"
loadsubDI 0 3
bNEA "branch_12_dr"
storeID 7 0
jumpA "block_8"

markA "branch_12_dr"
loadsubDI 0 4
bNEA "branch_12_ll"
discard
jumpA "block_11"

markA "branch_12_ll"
loadsubDI 0 5
bNEA "branch_12_lr"
discard
jumpA "block_11"

markA "branch_12_lr"
loadsubDI 0 6
bNEA "branch_12_ul"
storeID 1 0
cons 1
jumpA "block_13"

markA "branch_12_ul"
loadsubDI 0 7
bNEA "branch_12_ur"
storeID 0 0
cons 1
jumpA "block_13"

markA "branch_12_ur"
end

markA "block_13"
loadD 0
bNEA "branch_13_rl"
roll
jumpA "block_14"

markA "branch_13_rl"
loadsubDI 0 1
bNEA "branch_13_rr"
roll
jumpA "block_14"

markA "branch_13_rr"
loadsubDI 0 2
bNEA "branch_13_dl"
storeID 6 0
jumpA "block_8"

markA "branch_13_dl"
loadsubDI 0 3
bNEA "branch_13_dr"
storeID 7 0
jumpA "block_8"

markA "branch_13_dr"
loadsubDI 0 4
bNEA "branch_13_ll"
discard
jumpA "block_12"

markA "branch_13_ll"
loadsubDI 0 5
bNEA "branch_13_lr"
discard
jumpA "block_12"

markA "branch_13_lr"
loadsubDI 0 6
bNEA "branch_13_ul"
storeID 1 0
roll
jumpA "block_14"

markA "branch_13_ul"
loadsubDI 0 7
bNEA "branch_13_ur"
storeID 0 0
roll
jumpA "block_14"

markA "branch_13_ur"
end

markA "block_14"
loadD 0
bNEA "branch_14_rl"
copy I 0
jumpA "block_15"

markA "branch_14_rl"
loadsubDI 0 1
bNEA "branch_14_rr"
copy I 0
jumpA "block_15"

markA "branch_14_rr"
loadsubDI 0 2
bNEA "branch_14_dl"
storeID 6 0
jumpA "block_8"

markA "branch_14_dl"
loadsubDI 0 3
bNEA "branch_14_dr"
storeID 7 0
jumpA "block_8"

markA "branch_14_dr"
loadsubDI 0 4
bNEA "branch_14_ll"
lnot
jumpA "block_13"

markA "branch_14_ll"
loadsubDI 0 5
bNEA "branch_14_lr"
lnot
jumpA "block_13"

markA "branch_14_lr"
loadsubDI 0 6
bNEA "branch_14_ul"
storeID 1 0
copy I 0
jumpA "block_15"

markA "branch_14_ul"
loadsubDI 0 7
bNEA "branch_14_ur"
storeID 0 0
copy I 0
jumpA "block_15"

markA "branch_14_ur"
end

markA "block_15"
loadD 0
bNEA "branch_15_rl"
cons 3
jumpA "block_16"

markA "branch_15_rl"
loadsubDI 0 1
bNEA "branch_15_rr"
storeID 6 0
jumpA "block_5"

markA "branch_15_rr"
loadsubDI 0 2
bNEA "branch_15_dl"
storeID 6 0
jumpA "block_8"

markA "branch_15_dl"
loadsubDI 0 3
bNEA "branch_15_dr"
storeID 7 0
jumpA "block_8"

markA "branch_15_dr"
loadsubDI 0 4
bNEA "branch_15_ll"
jumpA "block_38"

markA "branch_15_ll"
loadsubDI 0 5
bNEA "branch_15_lr"
div
jumpA "block_14"

markA "branch_15_lr"
loadsubDI 0 6
bNEA "branch_15_ul"
jumpA "block_5"

markA "branch_15_ul"
loadsubDI 0 7
bNEA "branch_15_ur"
storeID 0 0
cons 3
jumpA "block_16"

markA "branch_15_ur"
end

markA "block_16"
loadD 0
bNEA "branch_16_rl"
cons 2
jumpA "block_17"

markA "branch_16_rl"
loadsubDI 0 1
bNEA "branch_16_rr"
storeID 6 0
jumpA "block_5"

markA "branch_16_rr"
loadsubDI 0 2
bNEA "branch_16_dl"
storeID 6 0
jumpA "block_5"

markA "branch_16_dl"
loadsubDI 0 3
bNEA "branch_16_dr"
storeID 7 0
jumpA "block_5"

markA "branch_16_dr"
loadsubDI 0 4
bNEA "branch_16_ll"
discard
jumpA "block_15"

markA "branch_16_ll"
loadsubDI 0 5
bNEA "branch_16_lr"
discard
jumpA "block_15"

markA "branch_16_lr"
loadsubDI 0 6
bNEA "branch_16_ul"
jumpA "block_5"

markA "branch_16_ul"
loadsubDI 0 7
bNEA "branch_16_ur"
storeID 0 0
cons 2
jumpA "block_17"

markA "branch_16_ur"
end

markA "block_17"
loadD 0
bNEA "branch_17_rl"
roll
jumpA "block_18"

markA "branch_17_rl"
loadsubDI 0 1
bNEA "branch_17_rr"
roll
jumpA "block_18"

markA "branch_17_rr"
loadsubDI 0 2
bNEA "branch_17_dl"
storeID 6 0
jumpA "block_5"

markA "branch_17_dl"
loadsubDI 0 3
bNEA "branch_17_dr"
storeID 7 0
jumpA "block_5"

markA "branch_17_dr"
loadsubDI 0 4
bNEA "branch_17_ll"
discard
jumpA "block_16"

markA "branch_17_ll"
loadsubDI 0 5
bNEA "branch_17_lr"
discard
jumpA "block_16"

markA "branch_17_lr"
loadsubDI 0 6
bNEA "branch_17_ul"
storeID 1 0
roll
jumpA "block_18"

markA "branch_17_ul"
loadsubDI 0 7
bNEA "branch_17_ur"
storeID 0 0
roll
jumpA "block_18"

markA "branch_17_ur"
end

markA "block_18"
loadD 0
bNEA "branch_18_rl"
jumpA "block_20"

markA "branch_18_rl"
loadsubDI 0 1
bNEA "branch_18_rr"
jumpA "block_20"

markA "branch_18_rr"
loadsubDI 0 2
bNEA "branch_18_dl"
storeID 6 0
jumpA "block_5"

markA "branch_18_dl"
loadsubDI 0 3
bNEA "branch_18_dr"
storeID 7 0
jumpA "block_5"

markA "branch_18_dr"
loadsubDI 0 4
bNEA "branch_18_ll"
lnot
jumpA "block_17"

markA "branch_18_ll"
loadsubDI 0 5
bNEA "branch_18_lr"
lnot
jumpA "block_17"

markA "branch_18_lr"
loadsubDI 0 6
bNEA "branch_18_ul"
storeID 1 0
jumpA "block_20"

markA "branch_18_ul"
loadsubDI 0 7
bNEA "branch_18_ur"
storeID 0 0
jumpA "block_20"

markA "branch_18_ur"
end

markA "block_20"
loadD 0
bNEA "branch_20_rl"
copy I 0
jumpA "block_21"

markA "branch_20_rl"
loadsubDI 0 1
bNEA "branch_20_rr"
copy I 0
jumpA "block_21"

markA "branch_20_rr"
loadsubDI 0 2
bNEA "branch_20_dl"
jumpA "block_39"

markA "branch_20_dl"
loadsubDI 0 3
bNEA "branch_20_dr"
jumpA "block_39"

markA "branch_20_dr"
loadsubDI 0 4
bNEA "branch_20_ll"
jumpA "block_18"

markA "branch_20_ll"
loadsubDI 0 5
bNEA "branch_20_lr"
jumpA "block_18"

markA "branch_20_lr"
loadsubDI 0 6
bNEA "branch_20_ul"
storeID 1 0
copy I 0
jumpA "block_21"

markA "branch_20_ul"
loadsubDI 0 7
bNEA "branch_20_ur"
storeID 0 0
copy I 0
jumpA "block_21"

markA "branch_20_ur"
end

markA "block_21"
loadD 0
bNEA "branch_21_rl"
cons 1
jumpA "block_22"

markA "branch_21_rl"
loadsubDI 0 1
bNEA "branch_21_rr"
cons 1
jumpA "block_22"

markA "branch_21_rr"
loadsubDI 0 2
bNEA "branch_21_dl"
jumpA "block_39"

markA "branch_21_dl"
loadsubDI 0 3
bNEA "branch_21_dr"
jumpA "block_39"

markA "branch_21_dr"
loadsubDI 0 4
bNEA "branch_21_ll"
div
jumpA "block_20"

markA "branch_21_ll"
loadsubDI 0 5
bNEA "branch_21_lr"
div
jumpA "block_20"

markA "branch_21_lr"
loadsubDI 0 6
bNEA "branch_21_ul"
storeID 1 0
cons 1
jumpA "block_22"

markA "branch_21_ul"
loadsubDI 0 7
bNEA "branch_21_ur"
storeID 0 0
cons 1
jumpA "block_22"

markA "branch_21_ur"
end

markA "block_22"
loadD 0
bNEA "branch_22_rl"
lgt
jumpA "block_23"

markA "branch_22_rl"
loadsubDI 0 1
bNEA "branch_22_rr"
lgt
jumpA "block_23"

markA "branch_22_rr"
loadsubDI 0 2
bNEA "branch_22_dl"
inputDS
jumpA "block_33"

markA "branch_22_dl"
loadsubDI 0 3
bNEA "branch_22_dr"
inputDS
jumpA "block_33"

markA "branch_22_dr"
loadsubDI 0 4
bNEA "branch_22_ll"
discard
jumpA "block_21"

markA "branch_22_ll"
loadsubDI 0 5
bNEA "branch_22_lr"
discard
jumpA "block_21"

markA "branch_22_lr"
loadsubDI 0 6
bNEA "branch_22_ul"
storeID 1 0
lgt
jumpA "block_23"

markA "branch_22_ul"
loadsubDI 0 7
bNEA "branch_22_ur"
storeID 0 0
lgt
jumpA "block_23"

markA "branch_22_ur"
end

markA "block_23"
loadD 0
bNEA "branch_23_rl"
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_24"

markA "branch_23_rl"
loadsubDI 0 1
bNEA "branch_23_rr"
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_24"

markA "branch_23_rr"
loadsubDI 0 2
bNEA "branch_23_dl"
mul
jumpA "block_33"

markA "branch_23_dl"
loadsubDI 0 3
bNEA "branch_23_dr"
mul
jumpA "block_33"

markA "branch_23_dr"
loadsubDI 0 4
bNEA "branch_23_ll"
lgt
jumpA "block_22"

markA "branch_23_ll"
loadsubDI 0 5
bNEA "branch_23_lr"
lgt
jumpA "block_22"

markA "branch_23_lr"
loadsubDI 0 6
bNEA "branch_23_ul"
storeID 1 0
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_24"

markA "branch_23_ul"
loadsubDI 0 7
bNEA "branch_23_ur"
storeID 0 0
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_24"

markA "branch_23_ur"
end

markA "block_24"
loadD 0
bNEA "branch_24_rl"
jumpA "block_26"

markA "branch_24_rl"
loadsubDI 0 1
bNEA "branch_24_rr"
jumpA "block_26"

markA "branch_24_rr"
loadsubDI 0 2
bNEA "branch_24_dl"
cons 1
jumpA "block_34"

markA "branch_24_dl"
loadsubDI 0 3
bNEA "branch_24_dr"
cons 1
jumpA "block_34"

markA "branch_24_dr"
loadsubDI 0 4
bNEA "branch_24_ll"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_23"

markA "branch_24_ll"
loadsubDI 0 5
bNEA "branch_24_lr"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_23"

markA "branch_24_lr"
loadsubDI 0 6
bNEA "branch_24_ul"
storeID 1 0
jumpA "block_26"

markA "branch_24_ul"
loadsubDI 0 7
bNEA "branch_24_ur"
storeID 0 0
jumpA "block_26"

markA "branch_24_ur"
end

markA "block_26"
loadD 0
bNEA "branch_26_rl"
cons 3
jumpA "block_27"

markA "branch_26_rl"
loadsubDI 0 1
bNEA "branch_26_rr"
storeID 0 0
cons 3
jumpA "block_27"

markA "branch_26_rr"
loadsubDI 0 2
bNEA "branch_26_dl"
storeID 5 0
jumpA "block_24"

markA "branch_26_dl"
loadsubDI 0 3
bNEA "branch_26_dr"
storeID 4 0
jumpA "block_40"

markA "branch_26_dr"
loadsubDI 0 4
bNEA "branch_26_ll"
jumpA "block_40"

markA "branch_26_ll"
loadsubDI 0 5
bNEA "branch_26_lr"
jumpA "block_24"

markA "branch_26_lr"
loadsubDI 0 6
bNEA "branch_26_ul"
storeID 0 0
cons 3
jumpA "block_27"

markA "branch_26_ul"
loadsubDI 0 7
bNEA "branch_26_ur"
storeID 0 0
cons 3
jumpA "block_27"

markA "branch_26_ur"
end

markA "block_27"
loadD 0
bNEA "branch_27_rl"
cons 2
jumpA "block_28"

markA "branch_27_rl"
loadsubDI 0 1
bNEA "branch_27_rr"
jumpA "block_36"

markA "branch_27_rr"
loadsubDI 0 2
bNEA "branch_27_dl"
storeID 5 0
discard
jumpA "block_26"

markA "branch_27_dl"
loadsubDI 0 3
bNEA "branch_27_dr"
storeID 4 0
discard
jumpA "block_26"

markA "branch_27_dr"
loadsubDI 0 4
bNEA "branch_27_ll"
discard
jumpA "block_26"

markA "branch_27_ll"
loadsubDI 0 5
bNEA "branch_27_lr"
discard
jumpA "block_26"

markA "branch_27_lr"
loadsubDI 0 6
bNEA "branch_27_ul"
storeID 1 0
jumpA "block_36"

markA "branch_27_ul"
loadsubDI 0 7
bNEA "branch_27_ur"
storeID 0 0
cons 2
jumpA "block_28"

markA "branch_27_ur"
end

markA "block_28"
loadD 0
bNEA "branch_28_rl"
roll
jumpA "block_29"

markA "branch_28_rl"
loadsubDI 0 1
bNEA "branch_28_rr"
roll
jumpA "block_29"

markA "branch_28_rr"
loadsubDI 0 2
bNEA "branch_28_dl"
storeID 5 0
jumpA "block_27"

markA "branch_28_dl"
loadsubDI 0 3
bNEA "branch_28_dr"
storeID 4 0
jumpA "block_27"

markA "branch_28_dr"
loadsubDI 0 4
bNEA "branch_28_ll"
discard
jumpA "block_27"

markA "branch_28_ll"
loadsubDI 0 5
bNEA "branch_28_lr"
discard
jumpA "block_27"

markA "branch_28_lr"
loadsubDI 0 6
bNEA "branch_28_ul"
storeID 1 0
roll
jumpA "block_29"

markA "branch_28_ul"
loadsubDI 0 7
bNEA "branch_28_ur"
storeID 0 0
roll
jumpA "block_29"

markA "branch_28_ur"
end

markA "block_29"
loadD 0
bNEA "branch_29_rl"
storeID 3 0
discard
jumpA "block_36"

markA "branch_29_rl"
loadsubDI 0 1
bNEA "branch_29_rr"
storeID 2 0
discard
jumpA "block_36"

markA "branch_29_rr"
loadsubDI 0 2
bNEA "branch_29_dl"
discard
jumpA "block_36"

markA "branch_29_dl"
loadsubDI 0 3
bNEA "branch_29_dr"
discard
jumpA "block_36"

markA "branch_29_dr"
loadsubDI 0 4
bNEA "branch_29_ll"
lnot
jumpA "block_28"

markA "branch_29_ll"
loadsubDI 0 5
bNEA "branch_29_lr"
lnot
jumpA "block_28"

markA "branch_29_lr"
loadsubDI 0 6
bNEA "branch_29_ul"
storeID 2 0
discard
jumpA "block_36"

markA "branch_29_ul"
loadsubDI 0 7
bNEA "branch_29_ur"
storeID 3 0
discard
jumpA "block_36"

markA "branch_29_ur"
end

markA "block_31"
loadD 0
bNEA "branch_31_rl"
jumpA "block_15"

markA "branch_31_rl"
loadsubDI 0 1
bNEA "branch_31_rr"
jumpA "block_15"

markA "branch_31_rr"
loadsubDI 0 2
bNEA "branch_31_dl"
discard
jumpA "block_38"

markA "branch_31_dl"
loadsubDI 0 3
bNEA "branch_31_dr"
discard
jumpA "block_38"

markA "branch_31_dr"
loadsubDI 0 4
bNEA "branch_31_ll"
storeID 7 0
jumpA "block_5"

markA "branch_31_ll"
loadsubDI 0 5
bNEA "branch_31_lr"
storeID 6 0
jumpA "block_5"

markA "branch_31_lr"
loadsubDI 0 6
bNEA "branch_31_ul"
cons 1
jumpA "block_11"

markA "branch_31_ul"
loadsubDI 0 7
bNEA "branch_31_ur"
cons 1
jumpA "block_11"

markA "branch_31_ur"
end

markA "block_33"
loadD 0
bNEA "branch_33_rl"
div
jumpA "block_34"

markA "branch_33_rl"
loadsubDI 0 1
bNEA "branch_33_rr"
modI 4
mulI 2
loadD 0
add
modI 8
rstoreD 0
jumpA "block_40"

markA "branch_33_rr"
loadsubDI 0 2
bNEA "branch_33_dl"
jumpA "block_62"

markA "branch_33_dl"
loadsubDI 0 3
bNEA "branch_33_dr"
jumpA "block_46"

markA "branch_33_dr"
loadsubDI 0 4
bNEA "branch_33_ll"
div
jumpA "block_39"

markA "branch_33_ll"
loadsubDI 0 5
bNEA "branch_33_lr"
storeID 6 0
jumpA "block_20"

markA "branch_33_lr"
loadsubDI 0 6
bNEA "branch_33_ul"
mod
jumpA "block_22"

markA "branch_33_ul"
loadsubDI 0 7
bNEA "branch_33_ur"
outputDMaybeS
jumpA "block_23"

markA "branch_33_ur"
end

markA "block_34"
loadD 0
bNEA "branch_34_rl"
jumpA "block_26"

markA "branch_34_rl"
loadsubDI 0 1
bNEA "branch_34_rr"
jumpA "block_26"

markA "branch_34_rr"
loadsubDI 0 2
bNEA "branch_34_dl"
sub
jumpA "block_40"

markA "branch_34_dl"
loadsubDI 0 3
bNEA "branch_34_dr"
sub
jumpA "block_40"

markA "branch_34_dr"
loadsubDI 0 4
bNEA "branch_34_ll"
copy I 0
jumpA "block_33"

markA "branch_34_ll"
loadsubDI 0 5
bNEA "branch_34_lr"
copy I 0
jumpA "block_33"

markA "branch_34_lr"
loadsubDI 0 6
bNEA "branch_34_ul"
discard
jumpA "block_24"

markA "branch_34_ul"
loadsubDI 0 7
bNEA "branch_34_ur"
discard
jumpA "block_24"

markA "branch_34_ur"
end

markA "block_36"
loadD 0
bNEA "branch_36_rl"
storeID 3 0
discard
jumpA "block_41"

markA "branch_36_rl"
loadsubDI 0 1
bNEA "branch_36_rr"
storeID 2 0
discard
jumpA "block_41"

markA "branch_36_rr"
loadsubDI 0 2
bNEA "branch_36_dl"
discard
jumpA "block_41"

markA "branch_36_dl"
loadsubDI 0 3
bNEA "branch_36_dr"
discard
jumpA "block_41"

markA "branch_36_dr"
loadsubDI 0 4
bNEA "branch_36_ll"
jumpA "block_27"

markA "branch_36_ll"
loadsubDI 0 5
bNEA "branch_36_lr"
jumpA "block_27"

markA "branch_36_lr"
loadsubDI 0 6
bNEA "branch_36_ul"
cons 1
jumpA "block_29"

markA "branch_36_ul"
loadsubDI 0 7
bNEA "branch_36_ur"
cons 1
jumpA "block_29"

markA "branch_36_ur"
end

markA "block_37"
loadD 0
bNEA "branch_37_rl"
storeID 7 0
add
jumpA "block_0"

markA "branch_37_rl"
loadsubDI 0 1
bNEA "branch_37_rr"
storeID 6 0
add
jumpA "block_0"

markA "branch_37_rr"
loadsubDI 0 2
bNEA "branch_37_dl"
storeID 6 0
add
jumpA "block_0"

markA "branch_37_dl"
loadsubDI 0 3
bNEA "branch_37_dr"
storeID 7 0
add
jumpA "block_0"

markA "branch_37_dr"
loadsubDI 0 4
bNEA "branch_37_ll"
storeID 7 0
add
jumpA "block_0"

markA "branch_37_ll"
loadsubDI 0 5
bNEA "branch_37_lr"
storeID 6 0
add
jumpA "block_0"

markA "branch_37_lr"
loadsubDI 0 6
bNEA "branch_37_ul"
add
jumpA "block_0"

markA "branch_37_ul"
loadsubDI 0 7
bNEA "branch_37_ur"
add
jumpA "block_0"

markA "branch_37_ur"
end

markA "block_38"
loadD 0
bNEA "branch_38_rl"
jumpA "block_15"

markA "branch_38_rl"
loadsubDI 0 1
bNEA "branch_38_rr"
jumpA "block_15"

markA "branch_38_rr"
loadsubDI 0 2
bNEA "branch_38_dl"
cons 1
jumpA "block_42"

markA "branch_38_dl"
loadsubDI 0 3
bNEA "branch_38_dr"
cons 1
jumpA "block_42"

markA "branch_38_dr"
loadsubDI 0 4
bNEA "branch_38_ll"
storeID 7 0
jumpA "block_5"

markA "branch_38_ll"
loadsubDI 0 5
bNEA "branch_38_lr"
storeID 6 0
jumpA "block_5"

markA "branch_38_lr"
loadsubDI 0 6
bNEA "branch_38_ul"
cons 1
jumpA "block_31"

markA "branch_38_ul"
loadsubDI 0 7
bNEA "branch_38_ur"
cons 1
jumpA "block_31"

markA "branch_38_ur"
end

markA "block_39"
loadD 0
bNEA "branch_39_rl"
copy I 0
jumpA "block_33"

markA "branch_39_rl"
loadsubDI 0 1
bNEA "branch_39_rr"
copy I 0
jumpA "block_33"

markA "branch_39_rr"
loadsubDI 0 2
bNEA "branch_39_dl"
lnot
jumpA "block_44"

markA "branch_39_dl"
loadsubDI 0 3
bNEA "branch_39_dr"
storeID 7 0
jumpA "block_39"

markA "branch_39_dr"
loadsubDI 0 4
bNEA "branch_39_ll"
storeID 7 0
jumpA "block_21"

markA "branch_39_ll"
loadsubDI 0 5
bNEA "branch_39_lr"
storeID 6 0
jumpA "block_20"

markA "branch_39_lr"
loadsubDI 0 6
bNEA "branch_39_ul"
jumpA "block_20"

markA "branch_39_ul"
loadsubDI 0 7
bNEA "branch_39_ur"
jumpA "block_21"

markA "branch_39_ur"
end

markA "block_40"
loadD 0
bNEA "branch_40_rl"
jumpA "block_26"

markA "branch_40_rl"
loadsubDI 0 1
bNEA "branch_40_rr"
storeID 0 0
jumpA "block_26"

markA "branch_40_rr"
loadsubDI 0 2
bNEA "branch_40_dl"
cons 3
jumpA "block_49"

markA "branch_40_dl"
loadsubDI 0 3
bNEA "branch_40_dr"
cons 3
jumpA "block_49"

markA "branch_40_dr"
loadsubDI 0 4
bNEA "branch_40_ll"
jumpA "block_46"

markA "branch_40_ll"
loadsubDI 0 5
bNEA "branch_40_lr"
modI 2
loadD 0
bxor
rstoreD 0
jumpA "block_33"

markA "branch_40_lr"
loadsubDI 0 6
bNEA "branch_40_ul"
outputCMaybeS
jumpA "block_34"

markA "branch_40_ul"
loadsubDI 0 7
bNEA "branch_40_ur"
outputCMaybeS
jumpA "block_34"

markA "branch_40_ur"
end

markA "block_41"
loadD 0
bNEA "branch_41_rl"
storeID 3 0
outputDMaybeS
jumpA "block_58"

markA "branch_41_rl"
loadsubDI 0 1
bNEA "branch_41_rr"
storeID 2 0
outputDMaybeS
jumpA "block_58"

markA "branch_41_rr"
loadsubDI 0 2
bNEA "branch_41_dl"
outputDMaybeS
jumpA "block_58"

markA "branch_41_dl"
loadsubDI 0 3
bNEA "branch_41_dr"
outputDMaybeS
jumpA "block_58"

markA "branch_41_dr"
loadsubDI 0 4
bNEA "branch_41_ll"
storeID 7 0
cons 12
jumpA "block_36"

markA "branch_41_ll"
loadsubDI 0 5
bNEA "branch_41_lr"
storeID 6 0
cons 12
jumpA "block_36"

markA "branch_41_lr"
loadsubDI 0 6
bNEA "branch_41_ul"
cons 12
jumpA "block_36"

markA "branch_41_ul"
loadsubDI 0 7
bNEA "branch_41_ur"
cons 12
jumpA "block_36"

markA "branch_41_ur"
end

markA "block_42"
loadD 0
bNEA "branch_42_rl"
storeID 7 0
jumpA "block_5"

markA "branch_42_rl"
loadsubDI 0 1
bNEA "branch_42_rr"
storeID 6 0
jumpA "block_5"

markA "branch_42_rr"
loadsubDI 0 2
bNEA "branch_42_dl"
jumpA "block_52"

markA "branch_42_dl"
loadsubDI 0 3
bNEA "branch_42_dr"
jumpA "block_52"

markA "branch_42_dr"
loadsubDI 0 4
bNEA "branch_42_ll"
storeID 7 0
jumpA "block_5"

markA "branch_42_ll"
loadsubDI 0 5
bNEA "branch_42_lr"
storeID 6 0
jumpA "block_5"

markA "branch_42_lr"
loadsubDI 0 6
bNEA "branch_42_ul"
discard
jumpA "block_38"

markA "branch_42_ul"
loadsubDI 0 7
bNEA "branch_42_ur"
discard
jumpA "block_38"

markA "branch_42_ur"
end

markA "block_44"
loadD 0
bNEA "branch_44_rl"
jumpA "block_40"

markA "branch_44_rl"
loadsubDI 0 1
bNEA "branch_44_rr"
jumpA "block_40"

markA "branch_44_rr"
loadsubDI 0 2
bNEA "branch_44_dl"
discard
jumpA "block_46"

markA "branch_44_dl"
loadsubDI 0 3
bNEA "branch_44_dr"
discard
jumpA "block_46"

markA "branch_44_dr"
loadsubDI 0 4
bNEA "branch_44_ll"
storeID 7 0
jumpA "block_39"

markA "branch_44_ll"
loadsubDI 0 5
bNEA "branch_44_lr"
storeID 6 0
jumpA "block_39"

markA "branch_44_lr"
loadsubDI 0 6
bNEA "branch_44_ul"
roll
jumpA "block_39"

markA "branch_44_ul"
loadsubDI 0 7
bNEA "branch_44_ur"
roll
jumpA "block_39"

markA "branch_44_ur"
end

markA "block_46"
loadD 0
bNEA "branch_46_rl"
jumpA "block_40"

markA "branch_46_rl"
loadsubDI 0 1
bNEA "branch_46_rr"
jumpA "block_40"

markA "branch_46_rr"
loadsubDI 0 2
bNEA "branch_46_dl"
jumpA "block_48"

markA "branch_46_dl"
loadsubDI 0 3
bNEA "branch_46_dr"
discard
jumpA "block_48"

markA "branch_46_dr"
loadsubDI 0 4
bNEA "branch_46_ll"
storeID 7 0
jumpA "block_39"

markA "branch_46_ll"
loadsubDI 0 5
bNEA "branch_46_lr"
storeID 6 0
jumpA "block_39"

markA "branch_46_lr"
loadsubDI 0 6
bNEA "branch_46_ul"
cons 2
jumpA "block_44"

markA "branch_46_ul"
loadsubDI 0 7
bNEA "branch_46_ur"
jumpA "block_33"

markA "branch_46_ur"
end

markA "block_48"
loadD 0
bNEA "branch_48_rl"
jumpA "block_49"

markA "branch_48_rl"
loadsubDI 0 1
bNEA "branch_48_rr"
jumpA "block_49"

markA "branch_48_rr"
loadsubDI 0 2
bNEA "branch_48_dl"
cons 3
jumpA "block_55"

markA "branch_48_dl"
loadsubDI 0 3
bNEA "branch_48_dr"
outputDMaybeS
jumpA "block_54"

markA "branch_48_dr"
loadsubDI 0 4
bNEA "branch_48_ll"
storeID 7 0
jumpA "block_39"

markA "branch_48_ll"
loadsubDI 0 5
bNEA "branch_48_lr"
storeID 6 0
jumpA "block_39"

markA "branch_48_lr"
loadsubDI 0 6
bNEA "branch_48_ul"
cons 3
jumpA "block_46"

markA "branch_48_ul"
loadsubDI 0 7
bNEA "branch_48_ur"
cons 3
jumpA "block_46"

markA "branch_48_ur"
end

markA "block_49"
loadD 0
bNEA "branch_49_rl"
storeID 3 0
cons 2
jumpA "block_56"

markA "branch_49_rl"
loadsubDI 0 1
bNEA "branch_49_rr"
storeID 2 0
cons 2
jumpA "block_56"

markA "branch_49_rr"
loadsubDI 0 2
bNEA "branch_49_dl"
cons 2
jumpA "block_56"

markA "branch_49_dl"
loadsubDI 0 3
bNEA "branch_49_dr"
cons 2
jumpA "block_56"

markA "branch_49_dr"
loadsubDI 0 4
bNEA "branch_49_ll"
jumpA "block_48"

markA "branch_49_ll"
loadsubDI 0 5
bNEA "branch_49_lr"
jumpA "block_48"

markA "branch_49_lr"
loadsubDI 0 6
bNEA "branch_49_ul"
discard
jumpA "block_40"

markA "branch_49_ul"
loadsubDI 0 7
bNEA "branch_49_ur"
discard
jumpA "block_40"

markA "branch_49_ur"
end

markA "block_51"
end

markA "block_52"
loadD 0
bNEA "branch_52_rl"
storeID 3 0
outputDMaybeS
jumpA "block_51"

markA "branch_52_rl"
loadsubDI 0 1
bNEA "branch_52_rr"
storeID 2 0
outputDMaybeS
jumpA "block_51"

markA "branch_52_rr"
loadsubDI 0 2
bNEA "branch_52_dl"
outputDMaybeS
jumpA "block_51"

markA "branch_52_dl"
loadsubDI 0 3
bNEA "branch_52_dr"
outputDMaybeS
jumpA "block_51"

markA "branch_52_dr"
loadsubDI 0 4
bNEA "branch_52_ll"
storeID 7 0
jumpA "block_42"

markA "branch_52_ll"
loadsubDI 0 5
bNEA "branch_52_lr"
storeID 6 0
jumpA "block_42"

markA "branch_52_lr"
loadsubDI 0 6
bNEA "branch_52_ul"
jumpA "block_42"

markA "branch_52_ul"
loadsubDI 0 7
bNEA "branch_52_ur"
jumpA "block_42"

markA "branch_52_ur"
end

markA "block_54"
loadD 0
bNEA "branch_54_rl"
add
jumpA "block_55"

markA "branch_54_rl"
loadsubDI 0 1
bNEA "branch_54_rr"
add
jumpA "block_55"

markA "branch_54_rr"
loadsubDI 0 2
bNEA "branch_54_dl"
lnot
jumpA "block_60"

markA "branch_54_dl"
loadsubDI 0 3
bNEA "branch_54_dr"
lnot
jumpA "block_60"

markA "branch_54_dr"
loadsubDI 0 4
bNEA "branch_54_ll"
storeID 7 0
jumpA "block_39"

markA "branch_54_ll"
loadsubDI 0 5
bNEA "branch_54_lr"
storeID 6 0
jumpA "block_39"

markA "branch_54_lr"
loadsubDI 0 6
bNEA "branch_54_ul"
mul
jumpA "block_48"

markA "branch_54_ul"
loadsubDI 0 7
bNEA "branch_54_ur"
mul
jumpA "block_48"

markA "branch_54_ur"
end

markA "block_55"
loadD 0
bNEA "branch_55_rl"
jumpA "block_56"

markA "branch_55_rl"
loadsubDI 0 1
bNEA "branch_55_rr"
jumpA "block_56"

markA "branch_55_rr"
loadsubDI 0 2
bNEA "branch_55_dl"
sub
jumpA "block_61"

markA "branch_55_dl"
loadsubDI 0 3
bNEA "branch_55_dr"
sub
jumpA "block_61"

markA "branch_55_dr"
loadsubDI 0 4
bNEA "branch_55_ll"
inputCS
jumpA "block_54"

markA "branch_55_ll"
loadsubDI 0 5
bNEA "branch_55_lr"
inputCS
jumpA "block_54"

markA "branch_55_lr"
loadsubDI 0 6
bNEA "branch_55_ul"
discard
jumpA "block_48"

markA "branch_55_ul"
loadsubDI 0 7
bNEA "branch_55_ur"
discard
jumpA "block_48"

markA "branch_55_ur"
end

markA "block_56"
loadD 0
bNEA "branch_56_rl"
storeID 3 0
roll
jumpA "block_63"

markA "branch_56_rl"
loadsubDI 0 1
bNEA "branch_56_rr"
storeID 2 0
roll
jumpA "block_63"

markA "branch_56_rr"
loadsubDI 0 2
bNEA "branch_56_dl"
roll
jumpA "block_63"

markA "branch_56_dl"
loadsubDI 0 3
bNEA "branch_56_dr"
roll
jumpA "block_63"

markA "branch_56_dr"
loadsubDI 0 4
bNEA "branch_56_ll"
jumpA "block_55"

markA "branch_56_ll"
loadsubDI 0 5
bNEA "branch_56_lr"
jumpA "block_55"

markA "branch_56_lr"
loadsubDI 0 6
bNEA "branch_56_ul"
discard
jumpA "block_49"

markA "branch_56_ul"
loadsubDI 0 7
bNEA "branch_56_ur"
discard
jumpA "block_49"

markA "branch_56_ur"
end

markA "block_58"
end

markA "block_60"
loadD 0
bNEA "branch_60_rl"
discard
jumpA "block_61"

markA "branch_60_rl"
loadsubDI 0 1
bNEA "branch_60_rr"
discard
jumpA "block_61"

markA "branch_60_rr"
loadsubDI 0 2
bNEA "branch_60_dl"
storeID 6 0
roll
jumpA "block_54"

markA "branch_60_dl"
loadsubDI 0 3
bNEA "branch_60_dr"
storeID 7 0
roll
jumpA "block_54"

markA "branch_60_dr"
loadsubDI 0 4
bNEA "branch_60_ll"
storeID 7 0
roll
jumpA "block_54"

markA "branch_60_ll"
loadsubDI 0 5
bNEA "branch_60_lr"
storeID 6 0
roll
jumpA "block_54"

markA "branch_60_lr"
loadsubDI 0 6
bNEA "branch_60_ul"
roll
jumpA "block_54"

markA "branch_60_ul"
loadsubDI 0 7
bNEA "branch_60_ur"
roll
jumpA "block_54"

markA "branch_60_ur"
end

markA "block_61"
loadD 0
bNEA "branch_61_rl"
discard
jumpA "block_62"

markA "branch_61_rl"
loadsubDI 0 1
bNEA "branch_61_rr"
discard
jumpA "block_62"

markA "branch_61_rr"
loadsubDI 0 2
bNEA "branch_61_dl"
storeID 5 0
cons 3
jumpA "block_60"

markA "branch_61_dl"
loadsubDI 0 3
bNEA "branch_61_dr"
storeID 5 0
cons 3
jumpA "block_60"

markA "branch_61_dr"
loadsubDI 0 4
bNEA "branch_61_ll"
storeID 5 0
cons 3
jumpA "block_60"

markA "branch_61_ll"
loadsubDI 0 5
bNEA "branch_61_lr"
cons 3
jumpA "block_60"

markA "branch_61_lr"
loadsubDI 0 6
bNEA "branch_61_ul"
outputCMaybeS
jumpA "block_55"

markA "branch_61_ul"
loadsubDI 0 7
bNEA "branch_61_ur"
outputCMaybeS
jumpA "block_55"

markA "branch_61_ur"
end

markA "block_62"
loadD 0
bNEA "branch_62_rl"
div
jumpA "block_63"

markA "branch_62_rl"
loadsubDI 0 1
bNEA "branch_62_rr"
storeID 0 0
div
jumpA "block_63"

markA "branch_62_rr"
loadsubDI 0 2
bNEA "branch_62_dl"
storeID 5 0
cons 4
jumpA "block_61"

markA "branch_62_dl"
loadsubDI 0 3
bNEA "branch_62_dr"
storeID 5 0
cons 4
jumpA "block_61"

markA "branch_62_dr"
loadsubDI 0 4
bNEA "branch_62_ll"
storeID 5 0
cons 4
jumpA "block_61"

markA "branch_62_ll"
loadsubDI 0 5
bNEA "branch_62_lr"
cons 4
jumpA "block_61"

markA "branch_62_lr"
loadsubDI 0 6
bNEA "branch_62_ul"
jumpA "block_33"

markA "branch_62_ul"
loadsubDI 0 7
bNEA "branch_62_ur"
jumpA "block_33"

markA "branch_62_ur"
end

markA "block_63"
loadD 0
bNEA "branch_63_rl"
storeID 4 0
copy I 0
jumpA "block_62"

markA "branch_63_rl"
loadsubDI 0 1
bNEA "branch_63_rr"
storeID 5 0
copy I 0
jumpA "block_62"

markA "branch_63_rr"
loadsubDI 0 2
bNEA "branch_63_dl"
storeID 5 0
copy I 0
jumpA "block_62"

markA "branch_63_dl"
loadsubDI 0 3
bNEA "branch_63_dr"
storeID 4 0
copy I 0
jumpA "block_62"

markA "branch_63_dr"
loadsubDI 0 4
bNEA "branch_63_ll"
copy I 0
jumpA "block_62"

markA "branch_63_ll"
loadsubDI 0 5
bNEA "branch_63_lr"
copy I 0
jumpA "block_62"

markA "branch_63_lr"
loadsubDI 0 6
bNEA "branch_63_ul"
lnot
jumpA "block_56"

markA "branch_63_ul"
loadsubDI 0 7
bNEA "branch_63_ur"
lnot
jumpA "block_56"

markA "branch_63_ur"
end
end
