[ ICF
    ( Mark
        ( MNatural 1 )
    )
, IAL
    ( SPure
        ( Indexed Move
            ( ImmediateO 1 )
        )
    )
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
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 5 )
    )
, ICF
    ( Mark
        ( MNatural 4 )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF ( Labeled Jump LTop )
, ICF
    ( Mark
        ( MNatural 5 )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Mark
        ( MNatural 6 )
    )
, ICF
    ( Labeled Jump
        ( LImmediate 1 )
    )
, ICF
    ( Mark
        ( MNatural 7 )
    )
, ICF
    ( Mark
        ( MNatural 0 )
    )
, End
]