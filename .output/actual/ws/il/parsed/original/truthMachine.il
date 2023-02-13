[ IAL
    ( SPure
        ( Cons 0 )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ILS ( MIO InputDec )
, ILS Load
, ICF
    ( Labeled ( Branch EZ )
        ( LArtificial "\x0" )
    )
, ICF
    ( Mark
        ( MArtificial "\x1" )
    )
, IAL
    ( SPure
        ( Cons 1 )
    )
, IAL ( SIO OutputDec )
, ICF
    ( Labeled Jump
        ( LArtificial "\x1" )
    )
, ICF
    ( Mark
        ( MArtificial "\x0" )
    )
, IAL
    ( SPure
        ( Cons 0 )
    )
, IAL ( SIO OutputDec )
, End
]