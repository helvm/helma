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
        ( Cons 10 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rr" )
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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_2_dl" ) EZ
    )
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
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
, ISM ( SPure Halibut )
, ICF
    ( Labeled
        ( LArtificial "block_3" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_2_dr" )
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
, ISM ( SPure Discard )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_lr" )
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
        ( BArtificial "branch_3_ul" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_ul" )
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
        ( BArtificial "branch_3_ur" ) EZ
    )
, ISM
    ( SPure ( Unary LNot ) )
, ICF
    ( Labeled
        ( LArtificial "block_2" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_3_ur" )
    )
, End
]