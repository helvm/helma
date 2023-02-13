[ ICF
    ( Mark
        ( MNatural 1 )
    )
, IAL ( SIO InputChar )
, ICF
    ( Mark
        ( MNatural 2 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ICF
    ( Mark
        ( MNatural 3 )
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
    ( Mark
        ( MNatural 4 )
    )
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 7 )
    )
, ICF
    ( Mark
        ( MNatural 5 )
    )
, IAL ( SPure Discard )
, ICF
    ( Mark
        ( MNatural 6 )
    )
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
    ( Mark
        ( MNatural 8 )
    )
, ICF
    ( Labeled Jump
        ( LImmediate 1 )
    )
, ICF
    ( Mark
        ( MNatural 9 )
    )
, ICF
    ( Mark
        ( MNatural 0 )
    )
, End
]