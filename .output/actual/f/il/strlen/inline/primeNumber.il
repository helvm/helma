[ Comment " writes all prime numbers between 0 and 100 "
, Inst
    ( ISM
        ( SPure
            ( Cons 99 )
        )
    )
, Inst
    ( ISM
        ( SPure
            ( Cons 9 )
        )
    )
, Lambda
    [ Inst
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
                    ( IImmediate 0 ) Copy
                )
            )
        )
    ]
, Lambda
    [ Inst
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
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Div ) )
        )
    , Inst
        ( ISM
            ( SPure ( Binary Mul ) )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
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
                        ( IImmediate 0 ) Copy
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
        , Lambda
            [ Inst
                ( ISM ( SPure Discard ) )
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
                            ( IImmediate 0 ) Copy
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
            ]
        , Cond
        , Inst
            ( ISM
                ( SPure
                    ( Cons 0 )
                )
            )
        , Lambda
            [ Inst
                ( ISM
                    ( SPure
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Inst
                ( ISM ( SIO OutputDec ) )
            , Inst
                ( ISM
                    ( SPure
                        ( Cons 32 )
                    )
                )
            , Inst
                ( ISM ( SIO OutputChar ) )
            ]
        , Cond
        ]
    , Cond
    ]
, While
]