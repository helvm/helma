[ Comment " writes all prime numbers between 0 and 100 "
, Inst
    ( IAL
        ( SPure
            ( Cons 99 )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 9 )
        )
    )
, Lambda
    [ Inst
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
                    ( IImmediate 0 ) Copy
                )
            )
        )
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
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
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
            ( SPure ( Binary Div ) )
        )
    , Inst
        ( IAL
            ( SPure ( Binary Mul ) )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
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
                        ( IImmediate 0 ) Copy
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
        , Lambda
            [ Inst
                ( IAL ( SPure Discard ) )
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
                            ( IImmediate 0 ) Copy
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
            ]
        , Cond
        , Inst
            ( IAL
                ( SPure
                    ( Cons 0 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary LEQ ) )
            )
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
                ( IAL ( SIO OutputDec ) )
            , Inst
                ( IAL
                    ( SPure
                        ( Cons 32 )
                    )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Inst
                ( IAL
                    ( SPure
                        ( Indexed
                            ( IImmediate 1 ) Move
                        )
                    )
                )
            ]
        , Cond
        ]
    , Cond
    ]
, While
]