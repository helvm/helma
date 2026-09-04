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
        ( Cons 11 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rl" )
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
, ISM
    ( SPure
        ( Cons 5 )
    )
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
        ( Cons 2 )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
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
, ISM
    ( SPure ( Binary Mul ) )
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
, ISM
    ( SPure ( Binary Mul ) )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_4_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_rl" )
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
        ( BArtificial "branch_4_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_rr" )
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_lr" )
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_5_ll" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_5_lr" )
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
, ILS ( MIO OutputChar )
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
, ILS ( MIO OutputChar )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_6_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
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
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_lr" )
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_7_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
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
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_6" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_7_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_8_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_9" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_rl" )
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
        ( BArtificial "branch_8_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_9" )
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
        ( BArtificial "branch_9_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_rl" )
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
        ( BArtificial "branch_9_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_rr" )
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
        ( BArtificial "branch_9_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_ll" )
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
        ( BArtificial "branch_9_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_lr" )
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
, ISM ( SPure Halibut )
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
, ISM ( SPure Halibut )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_10_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_9" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_ll" )
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
        ( BArtificial "branch_10_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_9" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_10_lr" )
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_12" ) Jump
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_12" ) Jump
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
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
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
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_10" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_lr" )
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_12_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
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
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_12_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_rl" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_rl" )
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
        ( BArtificial "branch_13_rr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_rr" )
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
        ( BArtificial "branch_13_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_12" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_ll" )
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
        ( BArtificial "branch_13_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_12" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_13_lr" )
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
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_14_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_13" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_14_lr" )
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
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_14" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_15_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_16_rl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_17" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_rl" )
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
        ( BArtificial "branch_16_rr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_17" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_rr" )
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_16_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_17_rl" ) EZ
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
        ( MArtificial "branch_17_rl" )
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
        ( BArtificial "branch_17_rr" ) EZ
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
        ( MArtificial "branch_17_rr" )
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
        ( BArtificial "branch_17_ll" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_ll" )
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
        ( BArtificial "branch_17_lr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_16" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_rl" ) EZ
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
        ( MArtificial "branch_18_rl" )
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
        ( BArtificial "branch_18_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_17" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_19_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_rl" )
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
        ( BArtificial "branch_19_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_rr" )
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_19_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_20_rl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_rl" )
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
        ( BArtificial "branch_20_rr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_rr" )
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
        ( LArtificial "block_19" ) Jump
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
        ( LArtificial "block_19" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_20_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_rl" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_rl" )
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
        ( BArtificial "branch_21_rr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_rr" )
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
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
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
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_21_lr" )
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
, ISM
    ( SPure
        ( Cons 43 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_dl" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_22_lr" )
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_24" ) Jump
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_24" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_23_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_24" )
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
        ( BArtificial "branch_24_rl" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_rl" )
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
        ( BArtificial "branch_24_rr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_rr" )
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
        ( BArtificial "branch_24_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_ll" )
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
        ( BArtificial "branch_24_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_lr" )
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
, ISM
    ( SPure
        ( Cons 61 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_25_rl" )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_25" ) Jump
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
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
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
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
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
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_ll" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_ll" )
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
        ( BArtificial "branch_28_lr" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_29" )
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
        ( BArtificial "branch_29_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_30" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_29_rl" )
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
        ( BArtificial "branch_29_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_29_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_30" )
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
        ( BArtificial "branch_30_dl" ) EZ
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
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_30_dl" )
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
        ( BArtificial "branch_30_dr" ) EZ
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
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_30_dr" )
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
        ( BArtificial "branch_30_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_30_ll" )
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
        ( BArtificial "branch_30_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_30_lr" )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_36_ll" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_36_lr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_rl" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_rl" )
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
        ( BArtificial "branch_37_rr" ) EZ
    )
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_rr" )
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_37_lr" )
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
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
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
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_rr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_39" )
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
        ( BArtificial "branch_39_rl" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_rl" )
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
        ( BArtificial "branch_39_rr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_rr" )
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
        ( BArtificial "branch_39_ll" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_43" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_ll" )
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
        ( BArtificial "branch_39_lr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_43" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_40" )
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
        ( BArtificial "branch_40_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 25 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_45" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_rr" )
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
        ( BArtificial "branch_40_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_ll" )
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
        ( BArtificial "branch_40_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_lr" )
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
        ( BArtificial "branch_40_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_41" )
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
        ( BArtificial "branch_41_rl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 13 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_rl" )
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
        ( BArtificial "branch_41_rr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 13 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_rr" )
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
        ( BArtificial "branch_41_ll" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_ll" )
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
        ( BArtificial "branch_41_lr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_lr" )
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
        ( BArtificial "branch_41_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_ul" )
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
        ( BArtificial "branch_41_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_42" )
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
        ( BArtificial "branch_42_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_43" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_rl" )
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
        ( BArtificial "branch_42_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_43" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_rr" )
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
        ( BArtificial "branch_42_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_41" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_ll" )
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
        ( BArtificial "branch_42_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_41" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_43" )
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
        ( BArtificial "branch_43_rl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_43_rl" )
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
        ( BArtificial "branch_43_rr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_43_rr" )
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
        ( BArtificial "branch_43_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 9 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_43_ll" )
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
        ( BArtificial "branch_43_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 9 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_43_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_44" )
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
        ( BArtificial "branch_44_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_rl" )
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
        ( BArtificial "branch_44_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_rr" )
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
        ( BArtificial "branch_44_ll" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_ll" )
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
        ( BArtificial "branch_44_lr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_45" )
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
        ( BArtificial "branch_45_ll" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_45_ll" )
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
        ( BArtificial "branch_45_lr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_45_lr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_46" )
    )
, End
]