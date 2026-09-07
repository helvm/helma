[ Comment " Factorial version 2 "
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Mul ) )
        )
    , Inst
        ( ISM
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
            ( ISM ( SIO OutputDec ) )
        ]
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Sub ) )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( ISM
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
    ( ISM
        ( SPure
            ( Cons 4 )
        )
    )
, Ref 5
, Fetch
, Exec
]