cons 0
cons 0
store

markA "block_0"
cons 0
load
cons 0
sub
bNEA "branch_0_rl"
cons 12
jumpA "block_5"

markA "branch_0_rl"
cons 0
load
cons 1
sub
bNEA "branch_0_rr"
cons 12
jumpA "block_5"

markA "branch_0_rr"
cons 0
load
cons 2
sub
bNEA "branch_0_dl"
cons 0
cons 1
store
cons 12
jumpA "block_5"

markA "branch_0_dl"
cons 0
load
cons 3
sub
bNEA "branch_0_dr"
cons 0
cons 0
store
cons 12
jumpA "block_5"

markA "branch_0_dr"
cons 0
load
cons 4
sub
bNEA "branch_0_ll"
cons 0
cons 0
store
cons 12
jumpA "block_5"

markA "branch_0_ll"
cons 0
load
cons 5
sub
bNEA "branch_0_lr"
cons 0
cons 1
store
cons 12
jumpA "block_5"

markA "branch_0_lr"
cons 0
load
cons 6
sub
bNEA "branch_0_ul"
cons 0
cons 1
store
cons 12
jumpA "block_5"

markA "branch_0_ul"
cons 0
load
cons 7
sub
bNEA "branch_0_ur"
cons 0
cons 0
store
cons 12
jumpA "block_5"

markA "branch_0_ur"
end

markA "block_2"
end

markA "block_3"
end

markA "block_4"
cons 0
load
cons 0
sub
bNEA "branch_4_rl"
cons 0
cons 3
store
copyI 0
jumpA "block_5"

markA "branch_4_rl"
cons 0
load
cons 1
sub
bNEA "branch_4_rr"
cons 0
cons 2
store
copyI 0
jumpA "block_5"

markA "branch_4_rr"
cons 0
load
cons 2
sub
bNEA "branch_4_dl"
copyI 0
jumpA "block_5"

markA "branch_4_dl"
cons 0
load
cons 3
sub
bNEA "branch_4_dr"
copyI 0
jumpA "block_5"

markA "branch_4_dr"
cons 0
load
cons 4
sub
bNEA "branch_4_ll"
cons 0
cons 7
store
outputCMaybeS
jumpA "block_3"

markA "branch_4_ll"
cons 0
load
cons 5
sub
bNEA "branch_4_lr"
cons 0
cons 6
store
outputCMaybeS
jumpA "block_3"

markA "branch_4_lr"
cons 0
load
cons 6
sub
bNEA "branch_4_ul"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ul"
cons 0
load
cons 7
sub
bNEA "branch_4_ur"
outputCMaybeS
jumpA "block_3"

markA "branch_4_ur"
end

markA "block_5"
cons 0
load
cons 0
sub
bNEA "branch_5_rl"
cons 0
cons 1
store
discard
jumpA "block_6"

markA "branch_5_rl"
cons 0
load
cons 1
sub
bNEA "branch_5_rr"
discard
jumpA "block_6"

markA "branch_5_rr"
cons 0
load
cons 2
sub
bNEA "branch_5_dl"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_5_dl"
cons 0
load
cons 3
sub
bNEA "branch_5_dr"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_5_dr"
cons 0
load
cons 4
sub
bNEA "branch_5_ll"
discard
jumpA "block_0"

markA "branch_5_ll"
cons 0
load
cons 5
sub
bNEA "branch_5_lr"
cons 0
cons 4
store
discard
jumpA "block_0"

markA "branch_5_lr"
cons 0
load
cons 6
sub
bNEA "branch_5_ul"
div
jumpA "block_4"

markA "branch_5_ul"
cons 0
load
cons 7
sub
bNEA "branch_5_ur"
div
jumpA "block_4"

markA "branch_5_ur"
end

markA "block_6"
cons 0
load
cons 0
sub
bNEA "branch_6_rl"
copyI 0
jumpA "block_2"

markA "branch_6_rl"
cons 0
load
cons 1
sub
bNEA "branch_6_rr"
copyI 0
jumpA "block_2"

markA "branch_6_rr"
cons 0
load
cons 2
sub
bNEA "branch_6_dl"
cons 0
cons 5
store
cons 2
jumpA "block_5"

markA "branch_6_dl"
cons 0
load
cons 3
sub
bNEA "branch_6_dr"
cons 0
cons 4
store
cons 2
jumpA "block_5"

markA "branch_6_dr"
cons 0
load
cons 4
sub
bNEA "branch_6_ll"
cons 2
jumpA "block_5"

markA "branch_6_ll"
cons 0
load
cons 5
sub
bNEA "branch_6_lr"
cons 2
jumpA "block_5"

markA "branch_6_lr"
cons 0
load
cons 6
sub
bNEA "branch_6_ul"
cons 0
cons 7
store
copyI 0
jumpA "block_2"

markA "branch_6_ul"
cons 0
load
cons 7
sub
bNEA "branch_6_ur"
copyI 0
jumpA "block_2"

markA "branch_6_ur"
end
end
