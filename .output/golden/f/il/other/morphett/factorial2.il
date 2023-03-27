[ Comment " Factorial version 2 "
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Mul ) )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Exec
    ]
, Ref 0
, Store
, Lambda
    [ Lambda
        [ Inst
            ( IAL ( SIO OutputDec ) )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
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
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
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
        ]
    , Lambda
        [ Ref 0
        , Fetch
        ]
    , While
    , Ref 0
    , Fetch
    , Exec
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 4 )
        )
    )
, Ref 5
, Fetch
, Exec
]