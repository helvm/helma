[ ICF
    ( Mark
        ( MNatural 1 )
    )
, IAL
    ( SPure
        ( Cons 0 )
    )
, IAL
    ( SPure
        ( Indexed Move
            ( ImmediateO 2 )
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
    ( Labeled ( Branch NE )
        ( LImmediate 4 )
    )
, ICF
    ( Mark
        ( MNatural 3 )
    )
, IAL
    ( SPure
        ( Cons 48 )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Labeled ( Branch NE ) LTop )
, ICF ( Labeled Jump LTop )
, ICF
    ( Mark
        ( MNatural 4 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ICF
    ( Mark
        ( MNatural 5 )
    )
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 7 )
    )
, ICF
    ( Mark
        ( MNatural 6 )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Labeled Jump
        ( LImmediate 11 )
    )
, ICF
    ( Mark
        ( MNatural 7 )
    )
, IAL
    ( SPure
        ( Cons 10 )
    )
, IAL
    ( SPure
        ( Binaries
            [ Mod
            , Div
            ]
        )
    )
, ICF
    ( Mark
        ( MArtificial "7" )
    )
, ICF
    ( Mark
        ( MNatural 8 )
    )
, IAL
    ( SPure
        ( Cons
            ( -48 )
        )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF
    ( Mark
        ( MNatural 9 )
    )
, IAL
    ( SPure
        ( Indexed Move
            ( ImmediateO 1 )
        )
    )
, ICF
    ( Mark
        ( MNatural 10 )
    )
, ICF
    ( Labeled Jump
        ( LImmediate 4 )
    )
, ICF
    ( Mark
        ( MNatural 11 )
    )
, IAL
    ( SPure
        ( Cons 12 )
    )
, ICF
    ( Labeled Jump
        ( LImmediate 13 )
    )
, ICF
    ( Mark
        ( MNatural 12 )
    )
, ICF ( Labeled Jump LTop )
, ICF
    ( Mark
        ( MNatural 13 )
    )
, IAL
    ( SPure
        ( Indexed Move
            ( ImmediateO 1 )
        )
    )
, ICF
    ( Mark
        ( MNatural 14 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ICF
    ( Mark
        ( MNatural 15 )
    )
, ICF
    ( Labeled ( Branch NE )
        ( LImmediate 17 )
    )
, ICF
    ( Mark
        ( MNatural 16 )
    )
, IAL
    ( SPure ( Binary Sub ) )
, ICF ( Labeled Jump LTop )
, ICF
    ( Mark
        ( MNatural 17 )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Mark
        ( MNatural 18 )
    )
, ICF
    ( Labeled Jump
        ( LImmediate 13 )
    )
, ICF
    ( Mark
        ( MNatural 19 )
    )
, ICF
    ( Mark
        ( MNatural 0 )
    )
, End
]