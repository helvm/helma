cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bNEA "branch_0_rl"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bNEA "branch_0_rr"
cons 10
jumpA "block_3"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bNEA "branch_0_dl"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bNEA "branch_0_dr"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_dr"
cons 0
load
cons 4
sub
bNEA "branch_0_ll"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_ll"
cons 0
load
cons 5
sub
bNEA "branch_0_lr"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_lr"
cons 0
load
cons 6
sub
bNEA "branch_0_ul"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_ul"
cons 0
load
cons 7
sub
bNEA "branch_0_ur"
cons 0
cons 1
store
cons 10
jumpA "block_3"

markA "branch_0_ur"
end

markA "block_2"
cons 0
load
cons 0
sub
bNEA "branch_2_rl"
cons 0
cons 3
store
roll
jumpA "block_3"

markA "branch_2_rl"
cons 0
load
cons 1
sub
bNEA "branch_2_rr"
cons 0
cons 2
store
roll
jumpA "block_3"

markA "branch_2_rr"
cons 0
load
cons 2
sub
bNEA "branch_2_dl"
roll
jumpA "block_3"

markA "branch_2_dl"
cons 0
load
cons 3
sub
bNEA "branch_2_dr"
roll
jumpA "block_3"

markA "branch_2_dr"
cons 0
load
cons 4
sub
bNEA "branch_2_ll"
cons 0
cons 3
store
roll
jumpA "block_3"

markA "branch_2_ll"
cons 0
load
cons 5
sub
bNEA "branch_2_lr"
cons 0
cons 2
store
roll
jumpA "block_3"

markA "branch_2_lr"
cons 0
load
cons 6
sub
bNEA "branch_2_ul"
cons 0
cons 2
store
roll
jumpA "block_3"

markA "branch_2_ul"
cons 0
load
cons 7
sub
bNEA "branch_2_ur"
cons 0
cons 3
store
roll
jumpA "block_3"

markA "branch_2_ur"
end

markA "block_3"
cons 0
load
cons 0
sub
bNEA "branch_3_rl"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_3_rl"
cons 0
load
cons 1
sub
bNEA "branch_3_rr"
cons 0
cons 5
store
discard
jumpA "block_0"

markA "branch_3_rr"
cons 0
load
cons 2
sub
bNEA "branch_3_dl"
cons 0
cons 5
store
discard
jumpA "block_0"

markA "branch_3_dl"
cons 0
load
cons 3
sub
bNEA "branch_3_dr"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_3_dr"
cons 0
load
cons 4
sub
bNEA "branch_3_ll"
discard
jumpA "block_0"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bNEA "branch_3_lr"
discard
jumpA "block_0"

markA "branch_3_lr"
cons 0
load
cons 6
sub
bNEA "branch_3_ul"
lnot
jumpA "block_2"

markA "branch_3_ul"
cons 0
load
cons 7
sub
bNEA "branch_3_ur"
lnot
jumpA "block_2"

markA "branch_3_ur"
end
end
