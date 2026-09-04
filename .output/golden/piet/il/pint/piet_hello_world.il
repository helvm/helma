[ ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Store
, ICF
    ( Mark
        ( MArtificial "block_0" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 72 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 72 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_dr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_1" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_2" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 101 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_rr" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_dl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_3" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_3_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_4" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_4_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 108 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_4_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 108 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_4_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_4_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_8" )
    )
, ICF
    ( Mark
        ( MArtificial "block_13" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_15" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_ul" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 33 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_16" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 100 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_lr" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_ul" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_ur" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_18" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 111 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_35" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 111 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_35" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_ll" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 111 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 111 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_20" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_dl" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_dr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_21" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_dr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_22" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_lr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_23" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_rl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_rr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_ll" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_lr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_25" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_lr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_25_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_26" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_26_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_27" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_27_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_27_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_28" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_rl" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_rr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 114 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_31" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_rl" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 119 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 119 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_ul" )
    )
, ICF
    ( Mark
        ( MArtificial "block_33" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_33_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_33_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_33_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_33_lr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_34" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_lr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mod ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Div ) )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Mul ) )
, ISM
    ( SPure ( Binary Add ) )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM ( SPure Halibut )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_35" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_dl" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_dr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_ll" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_lr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_35_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_35_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_36" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_dl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_dl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 3 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_dr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_dr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_ll" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_lr" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_35" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_35" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_37" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 32 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 32 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 32 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_38" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_rl" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_rr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_ll" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_lr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_lr" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_ul" )
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ILS Load
, ISM
    ( SPure
        ( Cons 7 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_ur" )
    )
, End
]