cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 52
jumpA "block_32"

markA "branch_0_rr"

markA "block_2"
cons 0
load
cons 1
sub
bEZA "branch_2_rr"
cons 8492
jumpA "block_40"

markA "branch_2_rr"

markA "block_32"
cons 0
load
cons 0
sub
bEZA "branch_32_rl"
copyI 0
jumpA "block_33"

markA "branch_32_rl"
cons 0
load
cons 1
sub
bEZA "branch_32_rr"
copyI 0
jumpA "block_33"

markA "branch_32_rr"
cons 0
load
cons 4
sub
bEZA "branch_32_ll"
discard
jumpA "block_0"

markA "branch_32_ll"
cons 0
load
cons 5
sub
bEZA "branch_32_lr"
discard
jumpA "block_0"

markA "branch_32_lr"

markA "block_33"
cons 0
load
cons 0
sub
bEZA "branch_33_rl"
mul
jumpA "block_34"

markA "branch_33_rl"
cons 0
load
cons 1
sub
bEZA "branch_33_rr"
mul
jumpA "block_34"

markA "branch_33_rr"
cons 0
load
cons 4
sub
bEZA "branch_33_ll"
div
jumpA "block_32"

markA "branch_33_ll"
cons 0
load
cons 5
sub
bEZA "branch_33_lr"
div
jumpA "block_32"

markA "branch_33_lr"

markA "block_34"
cons 0
load
cons 0
sub
bEZA "branch_34_rl"
jumpA "block_2"

markA "branch_34_rl"
cons 0
load
cons 1
sub
bEZA "branch_34_rr"
jumpA "block_2"

markA "branch_34_rr"
cons 0
load
cons 4
sub
bEZA "branch_34_ll"
mio outputdec
jumpA "block_33"

markA "branch_34_ll"
cons 0
load
cons 5
sub
bEZA "branch_34_lr"
mio outputdec
jumpA "block_33"

markA "branch_34_lr"

markA "block_37"
cons 0
load
cons 0
sub
bEZA "branch_37_rl"
cons 10
jumpA "block_42"

markA "branch_37_rl"
cons 0
load
cons 1
sub
bEZA "branch_37_rr"
cons 10
jumpA "block_42"

markA "branch_37_rr"

markA "block_38"
cons 0
load
cons 0
sub
bEZA "branch_38_rl"
cons 10
jumpA "block_54"

markA "branch_38_rl"
cons 0
load
cons 1
sub
bEZA "branch_38_rr"
cons 10
jumpA "block_54"

markA "branch_38_rr"

markA "block_39"

markA "block_40"
cons 0
load
cons 0
sub
bEZA "branch_40_rl"
jumpA "block_37"

markA "branch_40_rl"
cons 0
load
cons 1
sub
bEZA "branch_40_rr"
jumpA "block_37"

markA "branch_40_rr"
cons 0
load
cons 4
sub
bEZA "branch_40_ll"
discard
jumpA "block_2"

markA "branch_40_ll"
cons 0
load
cons 5
sub
bEZA "branch_40_lr"
discard
jumpA "block_2"

markA "branch_40_lr"

markA "block_42"
cons 0
load
cons 0
sub
bEZA "branch_42_rl"
copyI 0
jumpA "block_43"

markA "branch_42_rl"
cons 0
load
cons 1
sub
bEZA "branch_42_rr"
copyI 0
jumpA "block_43"

markA "branch_42_rr"
cons 0
load
cons 4
sub
bEZA "branch_42_ll"
discard
jumpA "block_37"

markA "branch_42_ll"
cons 0
load
cons 5
sub
bEZA "branch_42_lr"
discard
jumpA "block_37"

markA "branch_42_lr"

markA "block_43"
cons 0
load
cons 0
sub
bEZA "branch_43_rl"
mul
jumpA "block_44"

markA "branch_43_rl"
cons 0
load
cons 1
sub
bEZA "branch_43_rr"
mul
jumpA "block_44"

markA "branch_43_rr"
cons 0
load
cons 4
sub
bEZA "branch_43_ll"
div
jumpA "block_42"

markA "branch_43_ll"
cons 0
load
cons 5
sub
bEZA "branch_43_lr"
div
jumpA "block_42"

markA "branch_43_lr"

markA "block_44"
cons 0
load
cons 0
sub
bEZA "branch_44_rl"
copyI 0
jumpA "block_45"

markA "branch_44_rl"
cons 0
load
cons 1
sub
bEZA "branch_44_rr"
copyI 0
jumpA "block_45"

markA "branch_44_rr"
cons 0
load
cons 4
sub
bEZA "branch_44_ll"
mio outputdec
jumpA "block_43"

markA "branch_44_ll"
cons 0
load
cons 5
sub
bEZA "branch_44_lr"
mio outputdec
jumpA "block_43"

markA "branch_44_lr"

markA "block_45"
cons 0
load
cons 0
sub
bEZA "branch_45_rl"
mul
jumpA "block_46"

markA "branch_45_rl"
cons 0
load
cons 1
sub
bEZA "branch_45_rr"
mul
jumpA "block_46"

markA "branch_45_rr"
cons 0
load
cons 4
sub
bEZA "branch_45_ll"
div
jumpA "block_44"

markA "branch_45_ll"
cons 0
load
cons 5
sub
bEZA "branch_45_lr"
div
jumpA "block_44"

markA "branch_45_lr"

markA "block_46"
cons 0
load
cons 0
sub
bEZA "branch_46_rl"
mul
jumpA "block_47"

markA "branch_46_rl"
cons 0
load
cons 1
sub
bEZA "branch_46_rr"
mul
jumpA "block_47"

markA "branch_46_rr"
cons 0
load
cons 4
sub
bEZA "branch_46_ll"
mio outputdec
jumpA "block_45"

markA "branch_46_ll"
cons 0
load
cons 5
sub
bEZA "branch_46_lr"
mio outputdec
jumpA "block_45"

markA "branch_46_lr"

markA "block_47"
cons 0
load
cons 0
sub
bEZA "branch_47_rl"
cons 2
jumpA "block_48"

markA "branch_47_rl"
cons 0
load
cons 1
sub
bEZA "branch_47_rr"
cons 2
jumpA "block_48"

markA "branch_47_rr"
cons 0
load
cons 4
sub
bEZA "branch_47_ll"
mio outputdec
jumpA "block_46"

markA "branch_47_ll"
cons 0
load
cons 5
sub
bEZA "branch_47_lr"
mio outputdec
jumpA "block_46"

markA "branch_47_lr"

markA "block_48"
cons 0
load
cons 0
sub
bEZA "branch_48_rl"
cons 1
jumpA "block_49"

markA "branch_48_rl"
cons 0
load
cons 1
sub
bEZA "branch_48_rr"
cons 1
jumpA "block_49"

markA "branch_48_rr"
cons 0
load
cons 4
sub
bEZA "branch_48_ll"
discard
jumpA "block_47"

markA "branch_48_ll"
cons 0
load
cons 5
sub
bEZA "branch_48_lr"
discard
jumpA "block_47"

markA "branch_48_lr"

markA "block_49"
cons 0
load
cons 0
sub
bEZA "branch_49_rl"
halibut
jumpA "block_50"

markA "branch_49_rl"
cons 0
load
cons 1
sub
bEZA "branch_49_rr"
halibut
jumpA "block_50"

markA "branch_49_rr"
cons 0
load
cons 4
sub
bEZA "branch_49_ll"
discard
jumpA "block_48"

markA "branch_49_ll"
cons 0
load
cons 5
sub
bEZA "branch_49_lr"
discard
jumpA "block_48"

markA "branch_49_lr"

markA "block_50"
cons 0
load
cons 0
sub
bEZA "branch_50_rl"
div
jumpA "block_51"

markA "branch_50_rl"
cons 0
load
cons 1
sub
bEZA "branch_50_rr"
div
jumpA "block_51"

markA "branch_50_rr"
cons 0
load
cons 4
sub
bEZA "branch_50_ll"
lnot
jumpA "block_49"

markA "branch_50_ll"
cons 0
load
cons 5
sub
bEZA "branch_50_lr"
lnot
jumpA "block_49"

markA "branch_50_lr"

markA "block_51"
cons 0
load
cons 0
sub
bEZA "branch_51_rl"
mio outputdec
jumpA "block_52"

markA "branch_51_rl"
cons 0
load
cons 1
sub
bEZA "branch_51_rr"
mio outputdec
jumpA "block_52"

markA "branch_51_rr"
cons 0
load
cons 4
sub
bEZA "branch_51_ll"
copyI 0
jumpA "block_50"

markA "branch_51_ll"
cons 0
load
cons 5
sub
bEZA "branch_51_lr"
copyI 0
jumpA "block_50"

markA "branch_51_lr"

markA "block_52"
cons 0
load
cons 0
sub
bEZA "branch_52_rl"
jumpA "block_38"

markA "branch_52_rl"
cons 0
load
cons 1
sub
bEZA "branch_52_rr"
jumpA "block_38"

markA "branch_52_rr"
cons 0
load
cons 4
sub
bEZA "branch_52_ll"
mul
jumpA "block_51"

markA "branch_52_ll"
cons 0
load
cons 5
sub
bEZA "branch_52_lr"
mul
jumpA "block_51"

markA "branch_52_lr"

markA "block_54"
cons 0
load
cons 0
sub
bEZA "branch_54_rl"
mio outputchar
jumpA "block_39"

markA "branch_54_rl"
cons 0
load
cons 1
sub
bEZA "branch_54_rr"
mio outputchar
jumpA "block_39"

markA "branch_54_rr"
cons 0
load
cons 4
sub
bEZA "branch_54_ll"
discard
jumpA "block_38"

markA "branch_54_ll"
cons 0
load
cons 5
sub
bEZA "branch_54_lr"
discard
jumpA "block_38"

markA "branch_54_lr"
end
