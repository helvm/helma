[ Inst
    ( IAL
        ( SAL
            ( Cons 99 )
        )
    )
, Inst
    ( IAL
        ( SAL
            ( Cons 9 )
        )
    )
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Sub ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Div ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Mul ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Lambda
            [ Inst
                ( IAL ( SAL Discard ) )
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 1 )
                    )
                )
            , Inst
                ( IAL
                    ( SAL ( Binary Sub ) )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 2 Move )
                    )
                )
            ]
        , Cond
        , Inst
            ( IAL
                ( SAL
                    ( Cons 0 )
                )
            )
        , Lambda
            [ Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL ( SIO OutputDec ) )
            , Inst
                ( IAL
                    ( SAL
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