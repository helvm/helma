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
        ( Cons 9 )
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
        ( Cons 9 )
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
        ( LArtificial "block_8" ) Jump
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
        ( LArtificial "block_8" ) Jump
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 8 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
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
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( MArtificial "block_3" )
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
, ISM
    ( SPure
        ( Cons 11 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
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
, ISM
    ( SPure
        ( Cons 11 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
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
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
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
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_5" )
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
        ( BArtificial "branch_5_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_rl" )
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
        ( BArtificial "branch_5_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_rr" )
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
        ( BArtificial "branch_5_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_dl" )
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
        ( BArtificial "branch_5_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_dr" )
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
        ( BArtificial "branch_5_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_ll" )
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
        ( BArtificial "branch_5_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_lr" )
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
        ( BArtificial "branch_5_ul" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_ul" )
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
        ( BArtificial "branch_5_ur" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_6" )
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
        ( BArtificial "branch_6_rl" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_rl" )
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
        ( BArtificial "branch_6_rr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_rr" )
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
        ( BArtificial "branch_6_dl" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_dl" )
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
        ( BArtificial "branch_6_dr" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_dr" )
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
        ( BArtificial "branch_6_ll" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_ll" )
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
        ( BArtificial "branch_6_lr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_lr" )
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
        ( BArtificial "branch_6_ul" ) EZ
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
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_ul" )
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
        ( BArtificial "branch_6_ur" ) EZ
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
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_7" )
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
        ( BArtificial "branch_7_rl" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_rl" )
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
        ( BArtificial "branch_7_rr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_rr" )
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
        ( BArtificial "branch_7_dl" ) EZ
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
        ( LArtificial "block_12" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_dl" )
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
        ( BArtificial "branch_7_dr" ) EZ
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
        ( LArtificial "block_12" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_dr" )
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
        ( BArtificial "branch_7_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_ll" )
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
        ( BArtificial "branch_7_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_lr" )
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
        ( BArtificial "branch_7_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_ul" )
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
        ( BArtificial "branch_7_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_8" )
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
        ( BArtificial "branch_8_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_rr" )
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
        ( BArtificial "branch_8_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_17" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_dr" )
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
        ( BArtificial "branch_8_ul" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_ul" )
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
        ( BArtificial "branch_8_ur" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_10" )
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
        ( BArtificial "branch_10_rl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_rl" )
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
        ( BArtificial "branch_10_rr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_rr" )
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
        ( BArtificial "branch_10_dl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_dl" )
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
        ( BArtificial "branch_10_dr" ) EZ
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
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_dr" )
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
        ( BArtificial "branch_10_ul" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_ul" )
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
        ( BArtificial "branch_10_ur" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_11" )
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
        ( BArtificial "branch_11_rl" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_rl" )
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
        ( BArtificial "branch_11_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_rr" )
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
        ( BArtificial "branch_11_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 10 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_ll" )
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
        ( BArtificial "branch_11_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 10 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_lr" )
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
        ( BArtificial "branch_11_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_ul" )
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
        ( BArtificial "branch_11_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_12" )
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
        ( BArtificial "branch_12_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_rl" )
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
        ( BArtificial "branch_12_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_rr" )
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
        ( BArtificial "branch_12_dl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_dl" )
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
        ( BArtificial "branch_12_dr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_dr" )
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
        ( BArtificial "branch_12_ll" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_ll" )
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
        ( BArtificial "branch_12_lr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_lr" )
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
        ( BArtificial "branch_12_ul" ) EZ
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
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_ul" )
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
        ( BArtificial "branch_12_ur" ) EZ
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
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_13" )
    )
, ICF
    ( Mark
        ( MArtificial "block_14" )
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
        ( BArtificial "branch_14_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_rl" )
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
        ( BArtificial "branch_14_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_rr" )
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
        ( BArtificial "branch_14_dl" ) EZ
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
        ( MArtificial "branch_14_dl" )
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
        ( BArtificial "branch_14_dr" ) EZ
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
        ( MArtificial "branch_14_dr" )
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
        ( BArtificial "branch_14_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_ll" )
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
        ( BArtificial "branch_14_lr" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_lr" )
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
        ( BArtificial "branch_14_ul" ) EZ
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
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_ul" )
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
        ( BArtificial "branch_14_ur" ) EZ
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
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_ur" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_rl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_rl" )
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
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_ll" )
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
        ( BArtificial "branch_15_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_lr" )
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
        ( LArtificial "block_14" ) Jump
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
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_19" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_dl" )
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
        ( BArtificial "branch_16_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_dr" )
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
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_17" )
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
        ( BArtificial "branch_17_ul" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_ul" )
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
        ( BArtificial "branch_17_ur" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_19" )
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
        ( BArtificial "branch_19_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_dl" )
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
        ( BArtificial "branch_19_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_dr" )
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
        ( BArtificial "branch_19_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_ll" )
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
        ( BArtificial "branch_19_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_lr" )
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
        ( BArtificial "branch_19_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_ul" )
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
        ( BArtificial "branch_19_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_ur" )
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
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
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
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_19" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_19" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_ur" )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
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
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
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
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_ur" )
    )
, End
]