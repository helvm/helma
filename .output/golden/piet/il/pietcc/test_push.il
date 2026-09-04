cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bEZA "branch_0_rl"
cons 6
jumpA "block_1"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 6
jumpA "block_1"

markA "branch_0_rr"

markA "block_1"
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
end
