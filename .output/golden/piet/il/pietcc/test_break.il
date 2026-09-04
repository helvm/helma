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
        ( Cons 2 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_0_dl" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
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
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_1" ) Jump
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
        ( Cons 6 )
    )
, ISM
    ( SPure ( Binary Sub ) )
, ICF
    ( Branch
        ( BArtificial "branch_1_ul" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_ul" )
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
        ( BArtificial "branch_1_ur" ) EZ
    )
, ISM
    ( SPure ( Binary LGT ) )
, ICF
    ( Labeled
        ( LArtificial "block_0" ) Jump
    )
, ICF
    ( Mark
        ( MArtificial "branch_1_ur" )
    )
, End
]