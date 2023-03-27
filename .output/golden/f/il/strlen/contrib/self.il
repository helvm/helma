[ Lambda
    [ Str "'[,34,$!34,'],!" ]
, Inst
    ( IAL
        ( SPure
            ( Cons 91 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
, Inst
    ( IAL
        ( SPure
            ( Cons 34 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Exec
, Inst
    ( IAL
        ( SPure
            ( Cons 34 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
, Inst
    ( IAL
        ( SPure
            ( Cons 93 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
, Exec
]