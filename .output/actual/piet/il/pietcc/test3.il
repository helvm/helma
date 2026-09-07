cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 1
sub
bEZA "branch_0_rr"
cons 10
jumpA "block_3"

markA "branch_0_rr"

markA "block_2"
cons 0
load
cons 2
sub
bEZA "branch_2_dl"
halibut
jumpA "block_3"

markA "branch_2_dl"
cons 0
load
cons 3
sub
bEZA "branch_2_dr"
halibut
jumpA "block_3"

markA "branch_2_dr"

markA "block_3"
cons 0
load
cons 4
sub
bEZA "branch_3_ll"
discard
jumpA "block_0"

markA "branch_3_ll"
cons 0
load
cons 5
sub
bEZA "branch_3_lr"
discard
jumpA "block_0"

markA "branch_3_lr"
cons 0
load
cons 6
sub
bEZA "branch_3_ul"
lnot
jumpA "block_2"

markA "branch_3_ul"
cons 0
load
cons 7
sub
bEZA "branch_3_ur"
lnot
jumpA "block_2"

markA "branch_3_ur"
end
