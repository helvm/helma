[ Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
                ( SPure ( Binary Mul ) )
            )
        ]
    , Cond
    ]
, Ref 5
, Store
, Comment " fac() in false "
, Str "calculate the factorial of [1..8]: "
, Flush
, Inst
    ( IAL ( SIO InputChar ) )
, Flush
, Inst
    ( IAL
        ( SPure
            ( Cons 48 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Sub ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
]