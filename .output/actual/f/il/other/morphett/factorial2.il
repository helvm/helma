[ Comment " Factorial version 2 "
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary Mul ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
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
                ( SAL
                    ( SStatic 1 Move )
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
                    ( SStatic 1 Move )
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
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SAL ( Unary BNot ) )
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
        ( SAL
            ( Cons 4 )
        )
    )
, Ref 5
, Fetch
, Exec
]