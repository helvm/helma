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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_dl" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
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
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_37" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_dr" )
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
, ILS ( MIO InputDec )
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
, ILS ( MIO InputDec )
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
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
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
        ( LArtificial "block_0" ) Jump
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
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
, ISM
    ( SPure ( Binary Mod ) )
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
, ISM
    ( SPure ( Binary Mod ) )
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
        ( Cons 1 )
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
        ( Cons 1 )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_5_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure ( Binary Div ) )
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
, ISM
    ( SPure ( Binary Div ) )
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
, ISM
    ( SPure
        ( Cons 1 )
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
        ( Cons 1 )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
, ISM
    ( SPure ( Binary Sub ) )
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
, ISM
    ( SPure ( Binary Sub ) )
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
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
    ( SPure ( Binary LGT ) )
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_8_rr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_9" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_8_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_dl" )
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
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_8_ll" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_7" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_8_ll" )
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
, ILS ( MIO OutputChar )
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
    ( SPure ( Unary LNot ) )
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
    ( SPure ( Unary LNot ) )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_9_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_dl" )
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
        ( BArtificial "branch_9_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_9_dr" )
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
, ISM
    ( SPure ( Binary LGT ) )
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
, ISM
    ( SPure ( Binary LGT ) )
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
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_10_ll" ) EZ
    )
, ISM ( SPure Halibut )
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
, ISM ( SPure Halibut )
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
, ISM
    ( SPure
        ( Cons 2 )
    )
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
, ISM
    ( SPure
        ( Cons 2 )
    )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_11_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_dl" )
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
        ( BArtificial "branch_11_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_11_dr" )
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
, ISM
    ( SPure
        ( Cons 1 )
    )
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
, ISM
    ( SPure
        ( Cons 1 )
    )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_12_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
, ISM ( SPure Halibut )
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
, ISM ( SPure Halibut )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_13_ll" ) EZ
    )
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
        ( Indexed
            ( IImmediate 0 ) Copy
        )
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_14_rr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM
    ( SPure ( Unary LNot ) )
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
, ISM
    ( SPure ( Unary LNot ) )
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
        ( Cons 3 )
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
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_8" ) Jump
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_15_ll" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
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
, ISM
    ( SPure
        ( Cons 2 )
    )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM ( SPure Halibut )
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
, ISM ( SPure Halibut )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_17_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_dl" )
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
        ( BArtificial "branch_17_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_17_dr" )
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_rr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_18_rr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_17" ) Jump
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
        ( Cons 5 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_18_lr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
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
        ( MArtificial "branch_20_rr" )
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
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_18" ) Jump
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
, ISM
    ( SPure
        ( Cons 1 )
    )
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
, ISM
    ( SPure
        ( Cons 1 )
    )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_21_dl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
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
    ( SPure ( Binary Div ) )
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
    ( SPure ( Binary Div ) )
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
    ( SPure ( Binary LGT ) )
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_22_rr" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ILS ( MIO InputDec )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
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
, ISM ( SPure Discard )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_23_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ISM
    ( SPure ( Binary LGT ) )
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
, ISM
    ( SPure ( Binary LGT ) )
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
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_24_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_dl" )
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
        ( BArtificial "branch_24_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_dr" )
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
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_24_lr" )
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
, ISM
    ( SPure
        ( Cons 3 )
    )
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
        ( Cons 4 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_26_ll" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_24" ) Jump
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
    ( SPure
        ( Cons 2 )
    )
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
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
, ISM ( SPure Halibut )
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
, ISM ( SPure Halibut )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_28_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 5 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_dl" )
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
        ( BArtificial "branch_28_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_28_dr" )
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
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_29_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_29_dl" )
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
        ( BArtificial "branch_29_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_36" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_29_dr" )
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
        ( BArtificial "branch_29_ll" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_28" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_29_ll" )
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
    ( SPure ( Unary LNot ) )
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
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_31_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_dl" )
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
        ( BArtificial "branch_31_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_dr" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_ul" )
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
        ( BArtificial "branch_31_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_11" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_31_ur" )
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
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
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
        ( LArtificial "block_40" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_33_dl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_62" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_dl" )
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
        ( BArtificial "branch_33_dr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_dr" )
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
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_lr" )
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
        ( BArtificial "branch_33_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Mod ) )
, ICF
    ( Labeled
        ( LArtificial "block_22" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_ul" )
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
        ( BArtificial "branch_33_ur" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_23" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_33_ur" )
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
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_34_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_dl" )
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
        ( BArtificial "branch_34_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_dr" )
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
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_24" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_24" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_34_ur" )
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_41" ) Jump
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_41" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_27" ) Jump
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
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
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
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_29" ) Jump
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
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_37_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
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
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_15" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_38_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_dl" )
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
        ( BArtificial "branch_38_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_dr" )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
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
, ISM
    ( SPure
        ( Cons 1 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_31" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_38_ur" )
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_39_dl" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_dl" )
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
        ( BArtificial "branch_39_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_dr" )
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
        ( BArtificial "branch_39_ul" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_20" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_ul" )
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
        ( BArtificial "branch_39_ur" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_21" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_39_ur" )
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
        ( Cons 0 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_40_rl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_26" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_rl" )
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
        ( BArtificial "branch_40_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_dl" )
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
        ( BArtificial "branch_40_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_dr" )
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
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
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
        ( LArtificial "block_33" ) Jump
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
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_40_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_40_ul" )
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
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_34" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_41_dl" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_58" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_dl" )
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
        ( BArtificial "branch_41_dr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_58" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_41_dr" )
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
    ( SPure
        ( Cons 12 )
    )
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
    ( SPure
        ( Cons 12 )
    )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_42_dl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_52" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_dl" )
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
        ( BArtificial "branch_42_dr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_52" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_dr" )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_lr" )
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
        ( BArtificial "branch_42_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_ul" )
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
        ( BArtificial "branch_42_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_38" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_42_ur" )
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
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_44_dl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_dl" )
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
        ( BArtificial "branch_44_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_dr" )
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
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
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_lr" )
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
        ( BArtificial "branch_44_ul" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_ul" )
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
        ( BArtificial "branch_44_ur" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_44_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_46" )
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
        ( BArtificial "branch_46_rl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_rl" )
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
        ( BArtificial "branch_46_rr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_rr" )
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
        ( BArtificial "branch_46_dl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_dl" )
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
        ( BArtificial "branch_46_dr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_dr" )
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
        ( BArtificial "branch_46_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_ll" )
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
        ( BArtificial "branch_46_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_lr" )
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
        ( BArtificial "branch_46_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_44" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_ul" )
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
        ( BArtificial "branch_46_ur" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_46_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_48" )
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
        ( BArtificial "branch_48_rl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_rl" )
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
        ( BArtificial "branch_48_rr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_rr" )
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
        ( BArtificial "branch_48_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_dl" )
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
        ( BArtificial "branch_48_dr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_54" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_dr" )
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
        ( BArtificial "branch_48_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_ll" )
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
        ( BArtificial "branch_48_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_lr" )
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
        ( BArtificial "branch_48_ul" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_ul" )
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
        ( BArtificial "branch_48_ur" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_46" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_48_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_49" )
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
        ( BArtificial "branch_49_dl" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_dl" )
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
        ( BArtificial "branch_49_dr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 2 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_dr" )
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
        ( BArtificial "branch_49_ll" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_ll" )
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
        ( BArtificial "branch_49_lr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_lr" )
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
        ( BArtificial "branch_49_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_ul" )
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
        ( BArtificial "branch_49_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_40" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_49_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_51" )
    )
, ICF
    ( Mark
        ( MArtificial "block_52" )
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
        ( BArtificial "branch_52_dl" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_51" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_52_dl" )
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
        ( BArtificial "branch_52_dr" ) EZ
    )
, ILS ( MIO OutputDec )
, ICF
    ( Labeled
        ( LArtificial "block_51" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_52_dr" )
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
        ( BArtificial "branch_52_ul" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_52_ul" )
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
        ( BArtificial "branch_52_ur" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_42" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_52_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_54" )
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
        ( BArtificial "branch_54_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_rl" )
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
        ( BArtificial "branch_54_rr" ) EZ
    )
, ISM
    ( SPure ( Binary Add ) )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_rr" )
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
        ( BArtificial "branch_54_dl" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_60" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_dl" )
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
        ( BArtificial "branch_54_dr" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_60" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_dr" )
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
        ( BArtificial "branch_54_ll" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 7 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_ll" )
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
        ( BArtificial "branch_54_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 0 )
    )
, ISM
    ( SPure
        ( Cons 6 )
    )
, ILS Store
, ICF
    ( Labeled
        ( LArtificial "block_39" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_lr" )
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
        ( BArtificial "branch_54_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_ul" )
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
        ( BArtificial "branch_54_ur" ) EZ
    )
, ISM
    ( SPure ( Binary Mul ) )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_54_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_55" )
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
        ( BArtificial "branch_55_rl" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_rl" )
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
        ( BArtificial "branch_55_rr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_rr" )
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
        ( BArtificial "branch_55_dl" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_61" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_dl" )
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
        ( BArtificial "branch_55_dr" ) EZ
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled
        ( LArtificial "block_61" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_dr" )
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
        ( BArtificial "branch_55_ll" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_54" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_ll" )
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
        ( BArtificial "branch_55_lr" ) EZ
    )
, ILS ( MIO InputChar )
, ICF
    ( Labeled
        ( LArtificial "block_54" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_lr" )
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
        ( BArtificial "branch_55_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_ul" )
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
        ( BArtificial "branch_55_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_48" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_55_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_56" )
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
        ( BArtificial "branch_56_dl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_63" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_dl" )
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
        ( BArtificial "branch_56_dr" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_63" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_dr" )
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
        ( BArtificial "branch_56_ll" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_ll" )
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
        ( BArtificial "branch_56_lr" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_lr" )
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
        ( BArtificial "branch_56_ul" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_ul" )
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
        ( BArtificial "branch_56_ur" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_49" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_56_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_58" )
    )
, ICF
    ( Mark
        ( MArtificial "block_60" )
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
        ( BArtificial "branch_60_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_61" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_60_rl" )
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
        ( BArtificial "branch_60_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_61" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_60_rr" )
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
        ( BArtificial "branch_60_ul" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_54" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_60_ul" )
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
        ( BArtificial "branch_60_ur" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_54" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_60_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_61" )
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
        ( BArtificial "branch_61_rl" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_62" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_61_rl" )
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
        ( BArtificial "branch_61_rr" ) EZ
    )
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_62" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_61_rr" )
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
        ( BArtificial "branch_61_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 3 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_60" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_61_lr" )
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
        ( BArtificial "branch_61_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_61_ul" )
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
        ( BArtificial "branch_61_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_55" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_61_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_62" )
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
        ( BArtificial "branch_62_rl" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_63" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_62_rl" )
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
        ( BArtificial "branch_62_lr" ) EZ
    )
, ISM
    ( SPure
        ( Cons 4 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_61" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_62_lr" )
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
        ( BArtificial "branch_62_ul" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_62_ul" )
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
        ( BArtificial "branch_62_ur" ) EZ
    )
, ICF
    ( Labeled
        ( LArtificial "block_33" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_62_ur" )
    )
, ICF
    ( Mark
        ( MArtificial "block_63" )
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
        ( BArtificial "branch_63_ll" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_62" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_63_ll" )
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
        ( BArtificial "branch_63_lr" ) EZ
    )
, ISM
    ( SPure
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_62" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_63_lr" )
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
        ( BArtificial "branch_63_ul" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_63_ul" )
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
        ( BArtificial "branch_63_ur" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_56" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_63_ur" )
    )
, End
]