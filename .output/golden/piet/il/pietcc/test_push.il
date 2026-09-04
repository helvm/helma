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
        ( Cons 6 )
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
        ( Cons 6 )
    )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_0_rr" )
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
, End
]