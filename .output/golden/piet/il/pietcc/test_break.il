cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 2
sub
bEZA "branch_0_dl"
lgt
jumpA "block_1"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bEZA "branch_0_dr"
lgt
jumpA "block_1"

markA "branch_0_dr"

markA "block_1"
cons 0
load
cons 6
sub
bEZA "branch_1_ul"
lgt
jumpA "block_0"

markA "branch_1_ul"
cons 0
load
cons 7
sub
bEZA "branch_1_ur"
lgt
jumpA "block_0"

markA "branch_1_ur"
end
