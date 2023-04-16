[ Lambda
    [ Str "'[,34,$!34,'],!" ]
, Inst
    ( ISM
        ( SPure
            ( Cons 91 )
        )
    )
, Inst
    ( ISM ( SIO OutputChar ) )
, Inst
    ( ISM
        ( SPure
            ( Cons 34 )
        )
    )
, Inst
    ( ISM ( SIO OutputChar ) )
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Exec
, Inst
    ( ISM
        ( SPure
            ( Cons 34 )
        )
    )
, Inst
    ( ISM ( SIO OutputChar ) )
, Inst
    ( ISM
        ( SPure
            ( Cons 93 )
        )
    )
, Inst
    ( ISM ( SIO OutputChar ) )
, Exec
]