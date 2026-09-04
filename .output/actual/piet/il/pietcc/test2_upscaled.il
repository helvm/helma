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
        ( Cons 12 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
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
        ( Cons 12 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_5" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rr" )
    )
, ICF
    ( Mark
        ( MArtificial "block_2" )
    )
, ICF
    ( Mark
        ( MArtificial "block_3" )
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
        ( MArtificial "branch_4_dr" )
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
        ( BArtificial "branch_4_ul" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_ul" )
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
        ( BArtificial "branch_4_ur" ) EZ
    )
, ILS ( MIO OutputChar )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_4_ur" )
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
        ( Cons 1 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_5_rr" ) EZ
    )
, ISM ( SPure Discard )
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
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
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_5_ul" ) EZ
    )
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
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
, ISM
    ( SPure ( Binary Div ) )
, ICF
    ( Labeled
        ( LArtificial "block_4" ) Jump
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
        ( LArtificial "block_2" ) Jump
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
        ( LArtificial "block_2" ) Jump
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
    ( SPure
        ( Cons 2 )
    )
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
    ( SPure
        ( Cons 2 )
    )
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
        ( Indexed
            ( IImmediate 0 ) Copy
        )
    )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_6_ur" )
    )
, End
]