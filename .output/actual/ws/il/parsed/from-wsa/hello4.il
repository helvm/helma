[ IAL
    ( SPure
        ( Cons 0 )
    )
, IAL
    ( SPure
        ( Cons 33 )
    )
, IAL
    ( SPure
        ( Cons 100 )
    )
, IAL
    ( SPure
        ( Cons 108 )
    )
, IAL
    ( SPure
        ( Cons 114 )
    )
, IAL
    ( SPure
        ( Cons 111 )
    )
, IAL
    ( SPure
        ( Cons 119 )
    )
, IAL
    ( SPure
        ( Cons 32 )
    )
, IAL
    ( SPure
        ( Cons 44 )
    )
, IAL
    ( SPure
        ( Cons 111 )
    )
, IAL
    ( SPure
        ( Cons 108 )
    )
, IAL
    ( SPure
        ( Cons 108 )
    )
, IAL
    ( SPure
        ( Cons 101 )
    )
, IAL
    ( SPure
        ( Cons 72 )
    )
, ICF
    ( Labeled Call
        ( LArtificial "printsln" )
    )
, End
, ICF
    ( Mark
        ( MArtificial "prints" )
    )
, IAL
    ( SPure
        ( Indexed Copy
            ( ImmediateO 0 )
        )
    )
, ICF
    ( Labeled ( Branch EZ )
        ( LArtificial "prints_end" )
    )
, IAL ( SIO OutputChar )
, ICF
    ( Labeled Jump
        ( LArtificial "prints" )
    )
, ICF
    ( Mark
        ( MArtificial "prints_end" )
    )
, IAL ( SPure Discard )
, ICF Return
, ICF
    ( Mark
        ( MArtificial "printsln" )
    )
, ICF
    ( Labeled Call
        ( LArtificial "prints" )
    )
, IAL
    ( SPure
        ( Cons 10 )
    )
, IAL ( SIO OutputChar )
, ICF Return
]