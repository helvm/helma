cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 12
jumpA "block_5"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 12
jumpA "block_5"

markA "branch_0_rr"

markA "block_2"

markA "block_3"

markA "block_4"
cons 0
load
cons 2
sub
bEZA "branch_4_dl"
copyI 0
jumpA "block_5"

markA "branch_4_dl"
cons 0
load
cons 3
sub
bEZA "branch_4_dr"
copyI 0
jumpA "block_5"

markA "branch_4_dr"
cons 0
load
cons 6
sub
bEZA "branch_4_ul"
mio outputchar
jumpA "block_3"

markA "branch_4_ul"
cons 0
load
cons 7
sub
bEZA "branch_4_ur"
mio outputchar
jumpA "block_3"

markA "branch_4_ur"

markA "block_5"
cons 0
load
cons 1
sub
bEZA "branch_5_rr"
discard
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 4
sub
bEZA "branch_5_ll"
discard
jumpA "block_0"

markA "branch_5_ll"
cons 0
load
cons 6
sub
bEZA "branch_5_ul"
div
jumpA "block_4"

markA "branch_5_ul"
cons 0
load
cons 7
sub
bEZA "branch_5_ur"
div
jumpA "block_4"

markA "branch_5_ur"

markA "block_6"
cons 0
load
cons 0
sub
bEZA "branch_6_rl"
copyI 0
jumpA "block_2"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bEZA "branch_6_rr"
copyI 0
jumpA "block_2"

markA "branch_6_rr"
cons 0
load
cons 4
sub
bEZA "branch_6_ll"
cons 2
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bEZA "branch_6_lr"
cons 2
jumpA "block_5"

markA "branch_6_lr"
cons 0
load
cons 7
sub
bEZA "branch_6_ur"
copyI 0
jumpA "block_2"

markA "branch_6_ur"
end
