[ ICF
    ( Mark
        ( MNatural 1 )
    )
, IAL ( SIO InputChar )
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
, IAL ( SPure Discard )
, ICF
    ( Labeled Jump
        ( LImmediate 0 )
    )
, ICF
    ( Mark
        ( MNatural 2 )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Labeled Jump
        ( LImmediate 1 )
    )
, ICF
    ( Mark
        ( MNatural 3 )
    )
, ICF
    ( Mark
        ( MNatural 0 )
    )
, End
]