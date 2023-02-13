[ ICF
    ( Mark
        ( MNatural 1 )
    )
, IAL
    ( SPure
        ( Cons
            ( -1 )
        )
    )
, ICF
    ( Mark
        ( MNatural 2 )
    )
, IAL ( SIO InputChar )
, ICF
    ( Mark
        ( MNatural 3 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL
    ( SPure
        ( Cons
            ( -1 )
        )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 2 )
    )
, ICF
    ( Mark
        ( MNatural 4 )
    )
, IAL ( SPure Discard )
, ICF
    ( Mark
        ( MNatural 5 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, IAL
    ( SPure
        ( Cons
            ( -1 )
        )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 7 )
    )
, ICF
    ( Mark
        ( MNatural 6 )
    )
, IAL ( SPure Discard )
, ICF
    ( Labeled Jump
        ( LImmediate 0 )
    )
, ICF
    ( Mark
        ( MNatural 7 )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Labeled Jump
        ( LImmediate 5 )
    )
, ICF
    ( Mark
        ( MNatural 8 )
    )
, ICF
    ( Mark
        ( MNatural 0 )
    )
, End
]