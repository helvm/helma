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
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    ]
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
                ( Indexed Move
                    ( ImmediateO 2 )
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
                ( Indexed Move
                    ( ImmediateO 2 )
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
                ( Indexed Move
                    ( ImmediateO 2 )
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
                ( Indexed Move
                    ( ImmediateO 2 )
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
        , Lambda
            [ Inst
                ( IAL ( SPure Discard ) )
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
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Inst
                ( IAL
                    ( SPure
                        ( Indexed Move
                            ( ImmediateO 2 )
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
                ( IAL ( SIO OutputDec ) )
            , Inst
                ( IAL
                    ( SPure
                        ( Cons 32 )
                    )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            ]
        , Cond
        ]
    , Cond
    ]
, While
]