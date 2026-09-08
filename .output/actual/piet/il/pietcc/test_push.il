cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bNEA "branch_0_rl"
cons 6
jumpA "block_1"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bNEA "branch_0_rr"
cons 6
jumpA "block_1"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bNEA "branch_0_dl"
cons 0
cons 1
store
cons 6
jumpA "block_1"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bNEA "branch_0_dr"
cons 0
cons 0
store
cons 6
jumpA "block_1"

markA "branch_0_dr"
cons 0
load
cons 4
sub
bNEA "branch_0_ll"
cons 0
cons 0
store
cons 6
jumpA "block_1"

markA "branch_0_ll"
cons 0
load
cons 5
sub
bNEA "branch_0_lr"
cons 0
cons 1
store
cons 6
jumpA "block_1"

markA "branch_0_lr"
cons 0
load
cons 6
sub
bNEA "branch_0_ul"
cons 0
cons 1
store
cons 6
jumpA "block_1"

markA "branch_0_ul"
cons 0
load
cons 7
sub
bNEA "branch_0_ur"
cons 0
cons 0
store
cons 6
jumpA "block_1"

markA "branch_0_ur"
end

markA "block_1"
cons 0
load
cons 0
sub
bNEA "branch_1_rl"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_1_rl"
cons 0
load
cons 1
sub
bNEA "branch_1_rr"
cons 0
cons 5
store
discard
jumpA "block_0"

markA "branch_1_rr"
cons 0
load
cons 2
sub
bNEA "branch_1_dl"
cons 0
cons 5
store
discard
jumpA "block_0"

markA "branch_1_dl"
cons 0
load
cons 3
sub
bNEA "branch_1_dr"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_1_dr"
cons 0
load
cons 4
sub
bNEA "branch_1_ll"
discard
jumpA "block_0"

markA "branch_1_ll"
cons 0
load
cons 5
sub
bNEA "branch_1_lr"
discard
jumpA "block_0"

markA "branch_1_lr"
cons 0
load
cons 6
sub
bNEA "branch_1_ul"
cons 0
cons 5
store
discard
jumpA "block_0"

markA "branch_1_ul"
cons 0
load
cons 7
sub
bNEA "branch_1_ur"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_1_ur"
end
end
