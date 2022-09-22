[ Comment " writes all prime numbers between 0 and 100 "
, Inst
    ( IAL
        ( Cons 99 )
    )
, Inst
    ( IAL
        ( Cons 9 )
    )
, Lambda
    [ Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
    , Inst
        ( IAL ( Binary Div ) )
    , Inst
        ( IAL ( Binary Mul ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Lambda
            [ Inst ( IAL Discard )
            , Inst
                ( IAL
                    ( SStatic 1 Move )
                )
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Sub ) )
            , Inst
                ( IAL
                    ( SStatic 0 Copy )
                )
            , Inst
                ( IAL
                    ( SStatic 2 Move )
                )
            ]
        , Cond
        , Inst
            ( IAL
                ( Cons 0 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Lambda
            [ Inst
                ( IAL
                    ( SStatic 1 Move )
                )
            , Inst
                ( IAL
                    ( SStatic 0 Copy )
                )
            , Inst
                ( IAL ( SIO OutputDec ) )
            , Inst
                ( IAL
                    ( Cons 32 )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Inst
                ( IAL
                    ( SStatic 1 Move )
                )
            ]
        , Cond
        ]
    , Cond
    ]
, While
]